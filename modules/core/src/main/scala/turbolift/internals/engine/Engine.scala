package turbolift.internals.engine
import java.util.concurrent.TimeUnit
import scala.annotation.{tailrec, switch}
import turbolift.{!!, Computation, Signature, ComputationCases => CC}
import turbolift.io.{Fiber, Zipper, Warp, Snap, Outcome, Cause, Exceptions}
import turbolift.io.{OnceVar, CountDownLatch, CyclicBarrier, Mutex}
import turbolift.interpreter.{Interpreter, Continuation}
import turbolift.internals.executor.Executor
import turbolift.internals.engine.Tags
import turbolift.internals.engine.stacked.{StepCases => SC, Step, Stack, Store, Local, Prompt, FrameKind, OpPush, OpSplit, OpCascaded}
import turbolift.internals.engine.concurrent.{Bits, Blocker, FiberImpl, WarpImpl}
import turbolift.internals.engine.concurrent.util.{OnceVarImpl, CountDownLatchImpl, CyclicBarrierImpl, MutexImpl}
import Halt.{Retire => ThreadDisowned}
import Local.Syntax._
import Prompt.Syntax._
import Cause.{Cancelled => CancelPayload}
import Misc._


private sealed abstract class Engine0 extends Runnable:
  protected var currentFiber: FiberImpl = null.asInstanceOf[FiberImpl]
  protected var currentEnv: Env = null.asInstanceOf[Env]
  protected var currentTickLow: Int = 0
  protected var currentTickHigh: Int = 0


/*private[turbolift]*/ abstract class Engine extends Engine0:
  protected var savedTag: Int = 0
  protected var savedPayload: Any = null
  protected var savedStep: Step = null.asInstanceOf[Step]
  protected var savedStack: Stack = null.asInstanceOf[Stack]
  protected var savedStore: Store = null.asInstanceOf[Store]
  protected val pad1 = 0L
  protected val pad2 = 0


  def this(fiber: FiberImpl) = { this(); become(fiber) }


  final def runCurrent(): Halt =
    currentFiber.theOwnership = Bits.Ownership_Self
    currentTickLow = currentEnv.tickLow
    currentTickHigh = currentEnv.tickHigh
    if cancellationCheck() then
      currentFiber.suspendAsCancelled()
    outerLoop()

  
  //-------------------------------------------------------------------
  // Outer Loop
  //-------------------------------------------------------------------


  @tailrec private final def outerLoop(): Halt =
    val result =
      val tag     = currentFiber.suspendedTag
      val payload = currentFiber.suspendedPayload
      val step    = currentFiber.suspendedStep.nn
      val stack   = currentFiber.suspendedStack.nn
      val store   = currentFiber.suspendedStore.nn
      currentFiber.clearSuspension()
      try
        innerLoop(
          tag      = tag,
          payload  = payload,
          step     = step,
          stack    = stack,
          store    = store,
        )
      catch e =>
        // e.printStackTrace()
        val e2 = if e.isInstanceOf[Exceptions.Panic] then e else new Exceptions.Unhandled(e)
        val c = Cause(e2)
        endOfLoop(Bits.Completion_Failure, c, null)

    result match
      case Halt.Become => outerLoop()
      case halt: Halt => halt


  //-------------------------------------------------------------------
  // Inner Loop / 1st Stage
  //-------------------------------------------------------------------


  @tailrec private final def innerLoop(tag: Int, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop1st =
    inline def loopStep(payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop1st =
      innerLoop(step.tag, payload, step, stack, store)

    inline def loopComp(comp: Computation[?, ?], step: Step, stack: Stack, store: Store): Halt.Loop1st =
      innerLoop(comp.tag, comp, step, stack, store)

    inline def loopCancel(stack: Stack, store: Store): Halt.Loop1st =
      innerLoop(Tags.Step_Unwind, CancelPayload, Step.Cancel, stack, store)

    inline def loopThrow(throwable: Throwable, stack: Stack, store: Store): Halt.Loop1st =
      innerLoop(Tags.Step_Unwind, Cause(throwable), Step.Throw, stack, store)

    inline def loopTag(tag: Int, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop1st =
      val tag2 = if tag == Tags.MapFlat then payload.asInstanceOf[AnyComp].tag else step.tag
      innerLoop(tag2, payload, step, stack, store)

    if currentTickLow > 0 then
      currentTickLow -= 1
      (tag: @switch) match
        case Tags.MapFlat | Tags.MapPure =>
          val instr1 = payload.asInstanceOf[CC.Map[Any, Any, Any, Any]]
          val comp1 = instr1.comp
          (comp1.tag: @switch) match
            case Tags.Pure =>
              val instr2 = comp1.asInstanceOf[CC.Pure[Any]]
              val payload2 = instr1(instr2.value)
              loopTag(tag, payload2, step, stack, store)

            case Tags.Impure =>
              val instr2 = comp1.asInstanceOf[CC.Impure[Any]]
              val payload2 = instr1(instr2())
              loopTag(tag, payload2, step, stack, store)

            case Tags.Perform =>
              val instr2 = comp1.asInstanceOf[CC.Perform[Any, Any, Signature]]
              val (prompt, location) = stack.findSignature(instr2.sig)
              val comp2 = instr2(prompt)
              (comp2.tag: @switch) match
                case Tags.LocalGet =>
                  val local = store.getDeep(location)
                  val payload2 = instr1(local)
                  loopTag(tag, payload2, step, stack, store)

                case Tags.LocalPut =>
                  val instr3 = comp2.asInstanceOf[CC.LocalPut[Local]]
                  val store2 = store.setDeep(location, instr3.local)
                  val payload2 = instr1(())
                  loopTag(tag, payload2, step, stack, store2)

                case Tags.LocalUpdate =>
                  val instr3 = comp2.asInstanceOf[CC.LocalUpdate[Any, Local]]
                  val (value, store2) = store.updateDeep(location, instr3)
                  val payload2 = instr1(value)
                  loopTag(tag, payload2, step, stack, store2)

                case _ =>
                  val tag2 = tag + Tags.Step_MoreFlat - Tags.MapFlat
                  val step2 = SC.More(tag2, instr1, step)
                  loopComp(comp2, step2, stack, store)

            case _ =>
              val tag2 = tag + Tags.Step_MoreFlat - Tags.MapFlat
              val step2 = SC.More(tag2, instr1, step)
              loopComp(comp1, step2, stack, store)

        case Tags.Step_MoreFlat =>
          val instr = step.asInstanceOf[SC.More]
          val step2 = instr.next
          val comp2 = instr.fun(payload).asInstanceOf[AnyComp]
          loopComp(comp2, step2, stack, store)

        case Tags.Step_MorePure =>
          val instr = step.asInstanceOf[SC.More]
          val step2 = instr.next
          val payload2 = instr.fun(payload)
          loopStep(payload2, step2, stack, store)

        case Tags.Perform =>
          val instr = payload.asInstanceOf[CC.Perform[Any, Any, Signature]]
          val (prompt, location) = stack.findSignature(instr.sig)
          val comp2 = instr(prompt)
          (comp2.tag: @switch) match
            case Tags.LocalGet =>
              val local = store.getDeep(location)
              loopStep(local, step, stack, store)

            case Tags.LocalPut =>
              val instr2 = comp2.asInstanceOf[CC.LocalPut[Local]]
              val store2 = store.setDeep(location, instr2.local)
              loopStep((), step, stack, store2)

            case Tags.LocalUpdate =>
              val instr2 = comp2.asInstanceOf[CC.LocalUpdate[Any, Local]]
              val (value, store2) = store.updateDeep(location, instr2)
              loopStep(value, step, stack, store2)

            case _ => loopComp(comp2, step, stack, store)

        case Tags.Pure =>
          val instr = payload.asInstanceOf[CC.Pure[Any]]
          val payload2 = instr.value
          loopStep(payload2, step, stack, store)

        case Tags.Impure =>
          val instr = payload.asInstanceOf[CC.Impure[Any]]
          val payload2 = instr()
          loopStep(payload2, step, stack, store)

        case Tags.LocalGet =>
          val instr = payload.asInstanceOf[CC.LocalGet]
          val location = stack.locatePrompt(instr.prompt)
          val local = store.getDeep(location)
          loopStep(local, step, stack, store)

        case Tags.LocalPut =>
          val instr = payload.asInstanceOf[CC.LocalPut[Local]]
          val location = stack.locatePrompt(instr.prompt)
          val store2 = store.setDeep(location, instr.local)
          loopStep((), step, stack, store2)

        case Tags.LocalUpdate =>
          val instr = payload.asInstanceOf[CC.LocalUpdate[Any, Local]]
          val location = stack.locatePrompt(instr.prompt)
          val (value, store2) = store.updateDeep(location, instr)
          loopStep(value, step, stack, store2)

        case Tags.Sync =>
          val instr = payload.asInstanceOf[CC.Sync[Any, Any]]
          var result: Any = null
          var throwable: Throwable | Null = null
          try
            result = instr()
          catch
            case e => throwable = e
          if throwable == null then
            val payload2 = if instr.isAttempt then Right(result) else result
            loopStep(payload2, step, stack, store)
          else
            if instr.isAttempt then
              loopStep(Left(throwable), step, stack, store)
            else
              loopThrow(throwable.nn, stack, store)

        case _ =>
          loopMore(tag, payload, step, stack, store) match
            case Halt.Bounce =>
              val tag2     = savedTag
              val payload2 = savedPayload
              val step2    = savedStep
              val stack2   = savedStack
              val store2   = savedStore
              savedPayload = null
              savedStep = null.asInstanceOf[Step]
              savedStack = null.asInstanceOf[Stack]
              savedStore = null.asInstanceOf[Store]
              innerLoop(tag2, payload2, step2, stack2, store2)
            case halt: Halt.Loop1st => halt
    else
      if currentTickHigh > 0 then
        currentTickHigh -= 1
        currentTickLow = currentEnv.tickLow
        if cancellationCheck() then
          loopCancel(stack, store)
        else
          innerLoop(tag, payload, step, stack, store)
      else
        currentFiber.suspend(tag, payload, step, stack, store)
        Halt.Yield


  //-------------------------------------------------------------------
  // Inner Loop / 2nd Stage
  //-------------------------------------------------------------------


  private final def loopMore(tag: Int, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
    inline def innerLoop(tag: Int, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
      savedTag     = tag
      savedPayload = payload
      savedStep    = step
      savedStack   = stack
      savedStore   = store
      Halt.Bounce

    inline def loopStep(payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
      innerLoop(step.tag, payload, step, stack, store)

    inline def loopComp(comp: Computation[?, ?], step: Step, stack: Stack, store: Store): Halt.Loop2nd =
      innerLoop(comp.tag, comp, step, stack, store)

    inline def loopCancel(stack: Stack, store: Store): Halt.Loop2nd =
      innerLoop(Tags.Step_Unwind, CancelPayload, Step.Cancel, stack, store)

    inline def loopThrow(throwable: Throwable, stack: Stack, store: Store): Halt.Loop2nd =
      innerLoop(Tags.Step_Unwind, Cause(throwable), Step.Throw, stack, store)

    (tag: @switch) match
      case Tags.Intrinsic =>
        savedTag     = tag
        savedPayload = payload
        savedStep    = step
        savedStack   = stack
        savedStore   = store
        val instr = payload.asInstanceOf[CC.Intrinsic[Any, Any]]
        instr(this)

      case Tags.NotifyOnceVar =>
        val ovar = payload.asInstanceOf[OnceVarImpl]
        val value = ovar.theContent
        loopStep(value, step, stack, store)

      case Tags.NotifyZipper =>
        val waitee = payload.asInstanceOf[FiberImpl]
        val zipper = waitee.getOrMakeZipper
        loopStep(zipper, step, stack, store)

      case Tags.NotifyUnit =>
        loopStep((), step, stack, store)

      case Tags.NotifyEither =>
        payload.asInstanceOf[Either[Throwable, Any]] match
          case Right(a) => loopStep(a, step, stack, store)
          case Left(e) => loopThrow(e, stack, store)

      case Tags.NotifyBlocker =>
        val blocker = payload.asInstanceOf[Blocker]
        if blocker.isEither then
          val payload2 = blocker.getCompletion match
            case Bits.Completion_Success => Right(blocker.getResult)
            case Bits.Completion_Cancelled => Left(Exceptions.Cancelled)
            case Bits.Completion_Failure => Left(blocker.getThrowable)
          loopStep(payload2, step, stack, store)
        else
          blocker.getCompletion match
            case Bits.Completion_Success => loopStep(blocker.getResult, step, stack, store)
            case Bits.Completion_Cancelled =>
              currentFiber.cancelBySelf()
              loopCancel(stack, store)
            case Bits.Completion_Failure => loopThrow(blocker.getThrowable, stack, store)

      case Tags.Step_Push =>
        val instr = step.asInstanceOf[SC.Push]
        val step2 = instr.next
        val (stack2, store2) = OpPush.pushBase(stack, store, step2, instr.prompt, payload.asLocal)
        val comp2 = instr.body
        loopComp(comp2, SC.Pop, stack2, store2)

      case Tags.Step_Bridge =>
        val (stack2, store2, step2) = OpPush.drop(stack, store)
        refreshEnv(stack2, store2)
        loopStep(payload, step2, stack2, store2)

      case Tags.Step_Unwind =>
        val instr = step.asInstanceOf[SC.Unwind]
        if stack.canPop then
          val (stack2, store2, step2, prompt, frame, local) = OpPush.pop(stack, store)
          val fallthrough = if instr.kind.isPop then step2 else step
          if prompt.isIo then
            (frame.kind.unwrap: @switch) match
              case FrameKind.PLAIN =>
                refreshEnv(stack2, store2)
                loopStep(payload, fallthrough, stack2, store2)

              case FrameKind.GUARD =>
                val payload2 = instr.kind match
                  case Step.UnwindKind.Pop    => Snap.Success(payload)
                  case Step.UnwindKind.Abort  => Snap.Aborted(payload, instr.prompt.nn)
                  case Step.UnwindKind.Cancel => Snap.Cancelled
                  case Step.UnwindKind.Throw  => Snap.Failure(payload.asInstanceOf[Cause])
                refreshEnv(stack2, store2)
                loopStep(payload2, step2, stack2, store2)

              case FrameKind.WARP =>
                currentFiber.suspendStep(payload, fallthrough, stack2, store2)
                val warp = currentEnv.currentWarp.nn
                val tried = warp.exitMode match
                  case Bits.ExitMode_Cancel => warp.tryGetCancelledBy(currentFiber, currentEnv.isCancellable)
                  case Bits.ExitMode_Shutdown => warp.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable)
                  case _ => impossible //// this is a scoped warp, so it must have ExitMode
                tried match
                  case Bits.WaiterSubscribed => ThreadDisowned
                  case Bits.WaiterAlreadyCancelled => impossible //// Latch is set
                  case Bits.WaiteeAlreadyCompleted =>
                    currentFiber.clearSuspension()
                    refreshEnv(stack2, store2)
                    loopStep(payload, fallthrough, stack2, store2)

              case FrameKind.EXEC =>
                currentFiber.suspendStep(payload, fallthrough, stack2, store2)
                currentFiber.resume()
                ThreadDisowned

              case FrameKind.SUPPRESS =>
                refreshEnv(stack2, store2)
                if cancellationCheck() then
                  loopCancel(stack2, store2)
                else
                  loopStep(payload, fallthrough, stack2, store2)
            end match
          else
            if instr.kind.isPop then
              val comp2 = prompt.onReturn(payload, local)
              loopComp(comp2, step2, stack2, store2)
            else
              val step3 = if prompt == instr.prompt then step2 else step
              loopStep(payload, step3, stack2, store2)
        else
          val completion = instr.kind match
            case Step.UnwindKind.Pop    => Bits.Completion_Success
            case Step.UnwindKind.Abort  => impossible
            case Step.UnwindKind.Cancel => Bits.Completion_Cancelled
            case Step.UnwindKind.Throw  => Bits.Completion_Failure
          endOfLoop(completion, payload, stack)


  //-------------------------------------------------------------------
  // Intrinsics Aux
  //-------------------------------------------------------------------


  private final def intrinsicLoop(tag: Int, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
    savedPayload = payload
    savedTag = tag
    savedStep = step
    savedStack = stack
    savedStore = store
    Halt.Bounce

  private final def intrinsicLoopStep(value: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
    intrinsicLoop(step.tag, value, step, stack, store)

  private final def intrinsicLoopComp(comp: !![?, ?], step: Step, stack: Stack, store: Store): Halt.Loop2nd =
    intrinsicLoop(comp.tag, comp, step, stack, store)

  private final def intrinsicLoopStepRefreshEnv(value: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
    refreshEnv(stack, store)
    intrinsicLoopStep(value, step, stack, store)

  private final def intrinsicLoopCompRefreshEnv(comp: !![?, ?], step: Step, stack: Stack, store: Store): Halt.Loop2nd =
    refreshEnv(stack, store)
    intrinsicLoopComp(comp, step, stack, store)

  private final def intrinsicLoopCancel(stack: Stack, store: Store): Halt.Loop2nd =
    intrinsicLoop(Tags.Step_Unwind, CancelPayload, Step.Cancel, stack, store)


  //-------------------------------------------------------------------
  // Intrinsics
  //-------------------------------------------------------------------


  final def intrinsicDelimitPut[S](prompt: Interpreter.Untyped, body: AnyComp, local: S): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val location = stack.locatePrompt(prompt)
    val (stack2, store2) = OpPush.pushNested(stack, store, step, prompt, location, local.asLocal, FrameKind.plain)
    intrinsicLoopComp(body, SC.Pop, stack2, store2)


  final def intrinsicDelimitMod[S](prompt: Interpreter.Untyped, body: AnyComp, fun: S => S): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val location = stack.locatePrompt(prompt)
    val local2 = fun.asInstanceOf[Local => Local](store.getDeep(location))
    val (stack2, store2) = OpPush.pushNested(stack, store, step, prompt, location, local2, FrameKind.plain)
    intrinsicLoopComp(body, SC.Pop, stack2, store2)


  final def intrinsicAbort(prompt: Interpreter.Untyped, value: Any): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    intrinsicLoopStep(value, prompt.unwind, stack, store)


  final def intrinsicResume[A, B, S, U](cont0: Continuation[A, B, S, U], value: A): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val cont = cont0.asImpl
    val (stack2, store2) = OpSplit.merge(
      stackHi = cont.stack,
      storeHi = cont.store,
      stepMid = step,
      stackLo = stack,
      storeLo = store,
    )
    intrinsicLoopStepRefreshEnv(value, cont.step, stack2, store2)


  final def intrinsicResumePut[A, B, S, U](cont0: Continuation[A, B, S, U], value: A, local: S): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val cont = cont0.asImpl
    val (stack2, store2) = OpSplit.merge(
      stackHi = cont.stack,
      storeHi = cont.store.setDeepIfNotVoid(cont.location, local.asLocal),
      stepMid = step,
      stackLo = stack,
      storeLo = store,
    )
    intrinsicLoopStepRefreshEnv(value, cont.step, stack2, store2)


  final def intrinsicCapture[A, B, S, U](prompt: Interpreter.Untyped, fun: Continuation[A, B, S, U] => B !! U): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val location = stack.locatePrompt(prompt)
    val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(stack, store, location)
    //@#@THOV only the shallow paty of location2 is used
    val location2 = stackHi.locatePrompt(prompt)
    val cont = new ContImpl(stackHi, storeHi, step, location2)
    val comp2 = fun(cont.cast[A, B, S, U])
    intrinsicLoopCompRefreshEnv(comp2, stepMid, stackLo, storeLo)


  final def intrinsicCaptureGet[A, B, S, U](prompt: Interpreter.Untyped, fun: (Continuation[A, B, S, U], S) => B !! U): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val location = stack.locatePrompt(prompt)
    val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(stack, store, location)
    //@#@THOV only the shallow paty of location2 is used
    val location2 = stackHi.locatePrompt(prompt)
    val cont = new ContImpl(stackHi, storeHi, step, location2)
    val comp2 =
      val local = storeHi.getDeep(location2)
      fun(cont.cast[A, B, S, U], local.asInstanceOf[S])
    intrinsicLoopCompRefreshEnv(comp2, stepMid, stackLo, storeLo)


  final def intrinsicZipPar[A, B, C, U](lhs: A !! U, rhs: B !! U, fun: (A, B) => C): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    //@#@TODO Too conservative? Should check for `features.isParallel` at `mark`, instead of at stack top
    if stack.accumFeatures.isParallel && currentEnv.isParallelismRequested then
      val fiberLeft = currentFiber.createChild(Bits.ZipPar_Left)
      val fiberRight = currentFiber.createChild(Bits.ZipPar_Right)
      if currentFiber.tryStartRace(fiberLeft, fiberRight, currentEnv.isCancellable) then
        val (storeTmp, storeLeft) = OpCascaded.fork(stack, store)
        val (storeDown, storeRight) = OpCascaded.fork(stack, storeTmp)
        currentFiber.suspendForRace(fun, step, stack, storeDown)
        val stack2 = stack.makeFork
        fiberRight.suspend(rhs.tag, rhs, SC.Pop, stack2, storeRight)
        fiberRight.resume()
        becomeWithSameEnv(fiberLeft)
        intrinsicLoopComp(lhs, SC.Pop, stack2, storeLeft)
      else
        //// Must have been cancelled meanwhile
        intrinsicLoopCancel(stack, store)
    else
      //// Fallback to sequential
      val comp2 = lhs.zipWith(rhs)(fun)
      intrinsicLoopComp(comp2, step, stack, store)


  final def intrinsicOrPar[A, U](lhs: A !! U, rhs: A !! U): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    if stack.accumFeatures.isParallel && currentEnv.isParallelismRequested then
      val fiberLeft = currentFiber.createChild(Bits.OrPar_Left)
      val fiberRight = currentFiber.createChild(Bits.OrPar_Right)
      if currentFiber.tryStartRace(fiberLeft, fiberRight, currentEnv.isCancellable) then
        val (storeTmp, storeLeft) = OpCascaded.fork(stack, store)
        val (storeDown, storeRight) = OpCascaded.fork(stack, storeTmp)
        currentFiber.suspendForRace(null, step, stack, storeDown)
        val stack2 = stack.makeFork
        fiberRight.suspend(rhs.tag, rhs, SC.Pop, stack2, storeRight)
        fiberRight.resume()
        becomeWithSameEnv(fiberLeft)
        intrinsicLoopComp(lhs, SC.Pop, stack2, storeLeft)
      else
        //// Must have been cancelled meanwhile
        intrinsicLoopCancel(stack, store)
    else
      //// Fallback to sequential
      val comp2 = lhs ||! rhs
      intrinsicLoopComp(comp2, step, stack, store)


  final def intrinsicOrSeq[A, U](lhs: A !! U, rhsFun: () => A !! U): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val fiberLeft = currentFiber.createChild(Bits.OrSeq)
    if currentFiber.tryStartRaceOfOne(fiberLeft, currentEnv.isCancellable) then
      val (storeDown, storeLeft) = OpCascaded.fork(stack, store)
      currentFiber.suspendForRace(rhsFun, step, stack, storeDown)
      val stack2 = stack.makeFork
      becomeWithSameEnv(fiberLeft)
      intrinsicLoopComp(lhs, SC.Pop, stack2, storeLeft)
    else
      //// Must have been cancelled meanwhile
      intrinsicLoopCancel(stack, store)


  final def intrinsicHandle(body: AnyComp, interp: Interpreter.Untyped): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val prompt = interp
    for sig <- prompt.signatures do
      if stack.containsSignature(sig) then
        panic(s"Unsupported feature: shadowing effect ${sig}.")
    val comp2 = prompt.onInitial
    val step2 = new SC.Push(body, prompt, step)
    intrinsicLoopComp(comp2, step2, stack, store)


  final def intrinsicSnap[A, U](body: A !! U): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, Local.void, FrameKind.guard)
    intrinsicLoopComp(body, SC.Pop, stack2, store2)


  final def intrinsicUnsnap[A, U](snap: Snap[A]): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    //@#@TODO forbid uncancelling, it wouldnt work correctly anyway
    snap match
      case Snap.Success(payload2) => intrinsicLoopStep(payload2, step, stack, store)
      case Snap.Failure(payload2) => intrinsicLoopStep(payload2, Step.Throw, stack, store)
      case theAborted: Snap.Aborted => intrinsicLoopStep(theAborted.value, theAborted.prompt.unwind, stack, store)
      case Snap.Cancelled =>
        //@#@THOV It should be harmless to self-cancel a fiber, even when it's uncancellable?
        currentFiber.cancelBySelf()
        intrinsicLoopCancel(stack, store)


  final def intrinsicEnvAsk[A](fun: Env => A): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val value = fun(currentEnv)
    intrinsicLoopStep(value, step, stack, store)


  final def intrinsicEnvMod[A, U](fun: Env => Env, body: A !! U): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val env2 = fun(currentEnv)
    if currentEnv == env2 then
      intrinsicLoopComp(body, step, stack, store)
    else
      val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.plain)
      this.currentEnv = env2
      intrinsicLoopComp(body, SC.Pop, stack2, store2)


  final def intrinsicForkFiber[A, U](warp0: Warp | Null, comp: A !! U, name: String, callback: (Zipper.Untyped => Unit) | Null = null): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val warp = if warp0 != null then warp0.nn.asImpl else currentEnv.currentWarp.nn
    val (storeDown, storeFork) = OpCascaded.fork(stack, store)
    val stackFork = stack.makeFork
    val child = FiberImpl.createExplicit(warp, name, callback)
    child.suspend(comp.tag, comp, SC.Pop, stackFork, storeFork)
    if warp.tryAddFiber(child) then
      child.resume()
      intrinsicLoopStep(child, step, stack, storeDown)
    else
      child.suspendAsCancelled()
      intrinsicLoopStep(child, step, stack, store)


  final def intrinsicAwaitFiber[A, U](fiber: Fiber.Untyped, isCancel: Boolean, isVoid: Boolean): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val waitee = fiber.asImpl
    if currentFiber != waitee then
      val tag2 = if isVoid then Tags.NotifyUnit else Tags.NotifyZipper
      currentFiber.suspend(tag2, waitee, step, stack, store)
      val tried =
        if isCancel
        then waitee.tryGetCancelledBy(currentFiber, currentEnv.isCancellable)
        else waitee.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable)
      tried match
        case Bits.WaiterSubscribed => ThreadDisowned
        case Bits.WaiterAlreadyCancelled =>
          currentFiber.clearSuspension()
          intrinsicLoopCancel(stack, store)
        case Bits.WaiteeAlreadyCompleted =>
          currentFiber.clearSuspension()
          val payload2 = if isVoid then () else waitee.getOrMakeZipper
          intrinsicLoopStep(payload2, step, stack, store)
    else
      //// Ignoring `isCancellable` bcoz cancelling is by-self
      if isCancel then
        currentFiber.cancelBySelf()
        intrinsicLoopCancel(stack, store)
      else
        val zombie = new Blocker.Zombie(currentFiber)
        currentFiber.suspendStep(zombie, step, stack, store)
        if currentFiber.tryGetBlocked(currentEnv.isCancellable) then
          ThreadDisowned
        else
          currentFiber.clearSuspension()
          intrinsicLoopCancel(stack, store)


  final def intrinsicCurrentFiber(): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    intrinsicLoopStep(currentFiber, step, stack, store)


  final def intrinsicSpawnWarp[A, U](exitMode: Warp.ExitMode, body: A !! (U & Warp), name: String): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val warp = new WarpImpl(currentFiber, currentEnv.currentWarp, name, exitMode)
    val env2 = currentEnv.copy(currentWarp = warp)
    val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.warp)
    this.currentEnv = env2
    intrinsicLoopComp(body, SC.Pop, stack2, store2)


  final def intrinsicAwaitWarp(warp0: Warp, isCancel: Boolean): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val warp = warp0.asImpl
    currentFiber.suspendStep((), step, stack, store)
    val tried =
      if isCancel
      then warp.tryGetCancelledBy(currentFiber, currentEnv.isCancellable)
      else warp.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable)
    tried match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        intrinsicLoopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        intrinsicLoopStep((), step, stack, store)


  final def intrinsicAsync[A](callback: (Either[Throwable, A] => Unit) => Unit): Halt.Loop2nd =
    currentFiber.suspend(Tags.NotifyEither, null, savedStep, savedStack, savedStore)
    callback(currentFiber)
    ThreadDisowned


  final def intrinsicBlocking[A, B](thunk: () => A, isAttempt: Boolean): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val blocker = new Blocker.Interruptible(currentFiber, thunk, isAttempt)
    currentFiber.suspend(Tags.NotifyBlocker, blocker, step, stack, store)
    if currentFiber.tryGetBlocked(currentEnv.isCancellable) then
      blocker.block()
      ThreadDisowned
    else
      currentFiber.clearSuspension()
      intrinsicLoopCancel(stack, store)


  final def intrinsicSleep(length: Long, unit: TimeUnit = TimeUnit.MILLISECONDS): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val blocker = new Blocker.Sleeper(currentFiber)
    currentFiber.suspend(Tags.NotifyBlocker, blocker, step, stack, store)
    if currentFiber.tryGetBlocked(currentEnv.isCancellable) then
      blocker.sleep(length, unit)
      ThreadDisowned
    else
      currentFiber.clearSuspension()
      intrinsicLoopCancel(stack, store)


  final def intrinsicSuppress[A, U](body: A !! U, delta: Int): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val n1 = currentEnv.suppressions
    val n2 = 0.max(n1 + delta)
    if n1 == n2 then
      intrinsicLoopComp(body, step, stack, store)
    else
      val env2 = currentEnv.copy(suppressions = n2)
      val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.suppress)
      this.currentEnv = env2
      if cancellationCheck() then
        intrinsicLoopCancel(stack2, store2)
      else
        intrinsicLoopComp(body, SC.Pop, stack2, store2)


  final def intrinsicExecOn[A, U](exec: Executor, body: A !! U): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    if currentEnv.executor == exec then
      intrinsicLoopComp(body, step, stack, store)
    else
      val env2 = currentEnv.copy(executor = exec)
      val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.exec)
      currentFiber.suspendComp(body, SC.Pop, stack2, store2)
      currentFiber.resume()
      ThreadDisowned


  final def intrinsicYield: Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspend(step.tag, (), step, stack, store)
    Halt.Yield


  final def intrinsicAwaitOnceVar[A](ovar0: OnceVar.Get[A]): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val ovar = ovar0.asImpl
    val value = ovar.theContent
    if OnceVarImpl.Empty != value then
      intrinsicLoopStep(value, step, stack, store)
    else
      currentFiber.suspend(Tags.NotifyOnceVar, ovar, step, stack, store)
      ovar.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable) match
        case Bits.WaiterSubscribed => ThreadDisowned
        case Bits.WaiterAlreadyCancelled =>
          currentFiber.clearSuspension()
          intrinsicLoopCancel(stack, store)
        case Bits.WaiteeAlreadyCompleted =>
          currentFiber.clearSuspension()
          val value = ovar.theContent
          intrinsicLoopStep(value, step, stack, store)


  final def intrinsicAwaitCountDownLatch(latch: CountDownLatch): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspend(Tags.NotifyUnit, latch, step, stack, store)
    latch.asImpl.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        intrinsicLoopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        intrinsicLoopStep((), step, stack, store)


  final def intrinsicAwaitCyclicBarrier(barrier: CyclicBarrier): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspend(Tags.NotifyUnit, barrier, step, stack, store)
    barrier.asImpl.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        intrinsicLoopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        intrinsicLoopStep((), step, stack, store)


  final def intrinsicAcquireMutex(mutex: Mutex): Halt.Loop2nd =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspend(Tags.NotifyUnit, mutex, step, stack, store)
    mutex.asImpl.tryGetAcquiredBy(currentFiber, currentEnv.isCancellable) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        intrinsicLoopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        intrinsicLoopStep((), step, stack, store)


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


  private final def endOfLoop(completion: Int, payload: Any, stack: Stack | Null): Halt.Loop1st =
    currentFiber.doFinalize(completion, payload, stack) match
      case null => Halt.Retire
      case fiber2 => become(fiber2.nn); Halt.Become


  final def becomeClear(): Unit =
    currentFiber = null.asInstanceOf[FiberImpl]
    currentEnv = null.asInstanceOf[Env]


  final def become(fiber: FiberImpl): Unit =
    currentFiber = fiber
    refreshEnv(fiber.suspendedStack.nn, fiber.suspendedStore.nn) 


  final def getCurrentFiber: FiberImpl = currentFiber


  private final def becomeWithSameEnv(fiber: FiberImpl): Unit =
    currentFiber = fiber


  private final def refreshEnv(stack: Stack, store: Store): Unit =
    currentEnv = OpPush.findTopmostEnv(stack, store)


  private final def cancellationCheck(): Boolean =
    currentFiber.cancellationCheck(currentEnv.isCancellable)


/*private[turbolift]*/ object Engine:
  type IntrinsicResult = Halt.Loop2nd
