package turbolift.internals.engine
import java.util.concurrent.TimeUnit
import scala.annotation.{tailrec, switch}
import turbolift.{!!, Computation, Signature, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.data.{Snap, Outcome, Cause, Exceptions}
import turbolift.io.{Fiber, Zipper, Warp, OnceVar, EffectfulVar, CountDownLatch, CyclicBarrier, ReentrantLock, Mutex, Semaphore, Channel}
import turbolift.interpreter.{Interpreter, Continuation, Prompt}
import turbolift.internals.executor.Executor
import turbolift.internals.engine.stacked.{Stack, Store, Entry, Local, Location, FrameKind, OpPush, OpSplit, OpCascaded}
import turbolift.internals.engine.concurrent.{Bits, Blocker, Waitee, FiberImpl, WarpImpl}
import turbolift.internals.engine.concurrent.util.{OnceVarImpl, EffectfulVarImpl, CountDownLatchImpl, CyclicBarrierImpl, ReentrantLockImpl, SemaphoreImpl, ChannelImpl}
import Tag.{Retire => ThreadDisowned}
import Local.Syntax._
import Cause.{Cancelled => CancelPayload}
import Misc._


private sealed abstract class Engine0 extends Runnable:
  protected var currentFiber: FiberImpl = null.asInstanceOf[FiberImpl]
  protected var currentEnv: Env = null.asInstanceOf[Env]
  protected var currentTickLow: Int = 0
  protected var currentTickHigh: Int = 0


/*private[turbolift]*/ abstract class Engine extends Engine0:
  protected var savedPayload: Any = null
  protected var savedStep: Step = null.asInstanceOf[Step]
  protected var savedStack: Stack = null.asInstanceOf[Stack]
  protected var savedStore: Store = null.asInstanceOf[Store]
  protected val pad1 = 0L
  protected val pad2 = 0L


  def this(fiber: FiberImpl) = { this(); become(fiber) }


  final def runCurrent(): Halt =
    currentTickLow = currentEnv.tickLow
    currentTickHigh = currentEnv.tickHigh
    currentFiber.theWaiteeOrBlocker = null //// for those resumed by `finallyResumeAllWaiters`
    if cancellationCheck() then
      currentFiber.suspendAsCancelled()
    outerLoop()

  
  //-------------------------------------------------------------------
  // Outer Loop
  //-------------------------------------------------------------------


  @tailrec private final def outerLoop(): Halt =
    val result =
      val tag =
        val tag1          = currentFiber.suspendedTag
        this.savedPayload = currentFiber.suspendedPayload
        this.savedStep    = currentFiber.suspendedStep.nn
        this.savedStack   = currentFiber.suspendedStack.nn
        this.savedStore   = currentFiber.suspendedStore.nn
        currentFiber.clearSuspension()
        dispatchNotify(tag1)

      try
        middleLoop(tag)
      catch e =>
        // e.printStackTrace()
        val e2 = if e.isInstanceOf[Exceptions.Panic] then e else new Exceptions.Unhandled(e)
        val c = Cause(e2)
        endOfLoop(Bits.Completion_Failure, c)

    result match
      case Tag.Become => outerLoop()
      case Tag.Yield => Halt.Yield
      case Tag.Retire => Halt.Retire


  //-------------------------------------------------------------------
  // Middle Loop
  //-------------------------------------------------------------------


  @tailrec private final def middleLoop(tag: Tag): Tag =
    val tag2 =
      (tag: @switch) match
        case (
          Tag.FlatMap | Tag.PureMap | Tag.MoreFlat | Tag.MorePure |
          Tag.Perform | Tag.Pure | Tag.Impure |
          Tag.LocalGet | Tag.LocalGetsEff | Tag.LocalPut | Tag.LocalModify | Tag.LocalUpdate | Tag.Sync
        ) =>
          val payload = savedPayload
          val step    = savedStep
          val stack   = savedStack
          val store   = savedStore
          clearSaved()
          innerLoop(tag, payload, step, stack, store, !currentEnv.shadowMap.isEmpty)

        case Tag.Intrinsic =>
          val instr = savedPayload.asInstanceOf[CC.Intrinsic[Any, Any]]
          instr(this)

        case Tag.Unwind =>
          doUnwind()

        case Tag.Become | Tag.Yield | Tag.Retire => tag

    if tag2 < Tag.Become then
      middleLoop(tag2)
    else
      if tag2 < Tag.TickReset then
        tag2
      else
        val tag3 = tag2 - Tag.TickReset
        assert(tag3 < Tag.Become)
        if currentTickHigh > 0 then
          currentTickHigh -= 1
          currentTickLow = currentEnv.tickLow
          val tag4 =
            if cancellationCheck() then
              this.savedPayload = CancelPayload
              this.savedStep = Step.Cancel
              Tag.Unwind
            else
              tag3
          middleLoop(tag4)
        else
          currentFiber.suspend(tag3, savedPayload, savedStep, savedStack, savedStore)
          clearSaved()
          Tag.Yield


  //-------------------------------------------------------------------
  // Inner Loop
  //-------------------------------------------------------------------


  @tailrec private final def innerLoop(tag: Tag, payload: Any, step: Step, stack: Stack, store: Store, hasShadow: Boolean): Tag =
    inline def innerLoopStep(payload: Any, step: Step, store: Store): Tag =
      innerLoop(step.tag, payload, step, stack, store, hasShadow)

    inline def innerLoopComp(comp: Computation[?, ?], step: Step, store: Store): Tag =
      innerLoop(comp.tag, comp, step, stack, store, hasShadow)

    if currentTickLow > 0 then
      currentTickLow -= 1
      (tag: @switch) match
        case Tag.FlatMap =>
          val instr1 = payload.asInstanceOf[CC.FlatMap[Any, Any, Any]]
          val comp1 = instr1.comp
          (comp1.tag: @switch) match
            case Tag.Pure =>
              val instr2 = comp1.asInstanceOf[CC.Pure[Any]]
              val comp2 = instr1(instr2.value)
              innerLoopComp(comp2, step, store)

            case Tag.Impure =>
              val instr2 = comp1.asInstanceOf[CC.Impure[Any]]
              val comp2 = instr1(instr2())
              innerLoopComp(comp2, step, store)

            case Tag.Perform =>
              val instr2 = comp1.asInstanceOf[CC.Perform[Any, Any, Signature]]
              val entry = findEntryBySignature(instr2.sig, stack, hasShadow)
              val comp2 = instr2(entry.prompt)
              (comp2.tag: @switch) match
                case Tag.Pure =>
                  val instr3 = comp2.asInstanceOf[CC.Pure[Any]]
                  val comp3 = instr1(instr3.value)
                  innerLoopComp(comp3, step, store)

                case Tag.Impure =>
                  val instr3 = comp2.asInstanceOf[CC.Impure[Any]]
                  val comp3 = instr1(instr3())
                  innerLoopComp(comp3, step, store)

                case Tag.LocalGet =>
                  val local = store.deepGet(entry.storeIndex, entry.segmentDepth)
                  val comp3 = instr1(local)
                  innerLoopComp(comp3, step, store)

                case Tag.LocalGetsEff =>
                  val instr3 = comp2.asInstanceOf[CC.LocalGetsEff[Any, Any, Local]]
                  val local = store.deepGet(entry.storeIndex, entry.segmentDepth)
                  val comp3 = instr3(local)
                  val step2 = step.pushFlat(instr1)
                  innerLoopComp(comp3, step2, store)

                case Tag.LocalPut =>
                  val instr3 = comp2.asInstanceOf[CC.LocalPut[Local]]
                  val store2 = store.deepPut(entry.storeIndex, entry.segmentDepth, instr3.local)
                  val comp3 = instr1(())
                  innerLoopComp(comp3, step, store2)

                case Tag.LocalModify =>
                  val instr3 = comp2.asInstanceOf[CC.LocalModify[Local]]
                  val store2 = store.deepModify(entry.storeIndex, entry.segmentDepth, instr3)
                  val comp3 = instr1(())
                  innerLoopComp(comp3, step, store2)

                case Tag.LocalUpdate =>
                  val instr3 = comp2.asInstanceOf[CC.LocalUpdate[Any, Any, Local]]
                  val store2 = store.deepClone(entry.segmentDepth)
                  val value = store2.deepUpdateInPlace(entry.storeIndex, entry.segmentDepth, instr3)
                  val comp3 = instr1(value)
                  innerLoopComp(comp3, step, store2)

                case _ => innerLoopComp(comp2, step.pushFlat(instr1), store)

            case _ => innerLoopComp(comp1, step.pushFlat(instr1), store)

        case Tag.PureMap =>
          val instr1 = payload.asInstanceOf[CC.PureMap[Any, Any, Any]]
          val comp1 = instr1.comp
          (comp1.tag: @switch) match
            case Tag.Pure =>
              val instr2 = comp1.asInstanceOf[CC.Pure[Any]]
              val value2 = instr1(instr2.value)
              innerLoopStep(value2, step, store)

            case Tag.Impure =>
              val instr2 = comp1.asInstanceOf[CC.Impure[Any]]
              val value2 = instr1(instr2())
              innerLoopStep(value2, step, store)

            case Tag.Perform =>
              val instr2 = comp1.asInstanceOf[CC.Perform[Any, Any, Signature]]
              val entry = findEntryBySignature(instr2.sig, stack, hasShadow)
              val comp2 = instr2(entry.prompt)
              (comp2.tag: @switch) match
                case Tag.Pure =>
                  val instr3 = comp2.asInstanceOf[CC.Pure[Any]]
                  val value2 = instr1(instr3.value)
                  innerLoopStep(value2, step, store)

                case Tag.Impure =>
                  val instr3 = comp2.asInstanceOf[CC.Impure[Any]]
                  val value2 = instr1(instr3())
                  innerLoopStep(value2, step, store)

                case Tag.LocalGet =>
                  val local = store.deepGet(entry.storeIndex, entry.segmentDepth)
                  val value2 = instr1(local)
                  innerLoopStep(value2, step, store)

                case Tag.LocalGetsEff =>
                  val instr3 = comp2.asInstanceOf[CC.LocalGetsEff[Any, Any, Local]]
                  val local = store.deepGet(entry.storeIndex, entry.segmentDepth)
                  val comp3 = instr3(local)
                  val step2 = step.pushPure(instr1)
                  innerLoopComp(comp3, step2, store)

                case Tag.LocalPut =>
                  val instr3 = comp2.asInstanceOf[CC.LocalPut[Local]]
                  val store2 = store.deepPut(entry.storeIndex, entry.segmentDepth, instr3.local)
                  val value2 = instr1(())
                  innerLoopStep(value2, step, store2)

                case Tag.LocalModify =>
                  val instr3 = comp2.asInstanceOf[CC.LocalModify[Local]]
                  val store2 = store.deepModify(entry.storeIndex, entry.segmentDepth, instr3)
                  val value2 = instr1(())
                  innerLoopStep(value2, step, store2)

                case Tag.LocalUpdate =>
                  val instr3 = comp2.asInstanceOf[CC.LocalUpdate[Any, Any, Local]]
                  val store2 = store.deepClone(entry.segmentDepth)
                  val value = store2.deepUpdateInPlace(entry.storeIndex, entry.segmentDepth, instr3)
                  val value2 = instr1(value)
                  innerLoopStep(value2, step, store2)

                case _ => innerLoopComp(comp2, step.pushPure(instr1), store)

            case _ => innerLoopComp(comp1, step.pushPure(instr1), store)

        case Tag.MoreFlat =>
          val instr = step.asInstanceOf[Step.MoreFlat]
          val step2 = instr.next
          val comp2 = instr.fun(payload)
          innerLoopComp(comp2, step2, store)

        case Tag.MorePure =>
          val instr = step.asInstanceOf[Step.MorePure]
          val step2 = instr.next
          val value = instr.fun(payload)
          innerLoopStep(value, step2, store)

        case Tag.Perform =>
          val instr = payload.asInstanceOf[CC.Perform[Any, Any, Signature]]
          val entry = findEntryBySignature(instr.sig, stack, hasShadow)
          val comp2 = instr(entry.prompt)
          (comp2.tag: @switch) match
            case Tag.LocalGet =>
              val local = store.deepGet(entry.storeIndex, entry.segmentDepth)
              innerLoopStep(local, step, store)

            case Tag.LocalGetsEff =>
              val instr2 = comp2.asInstanceOf[CC.LocalGetsEff[Any, Any, Local]]
              val local = store.deepGet(entry.storeIndex, entry.segmentDepth)
              val comp3 = instr2(local)
              innerLoopComp(comp3, step, store)

            case Tag.LocalPut =>
              val instr2 = comp2.asInstanceOf[CC.LocalPut[Local]]
              val store2 = store.deepPut(entry.storeIndex, entry.segmentDepth, instr2.local)
              innerLoopStep((), step, store2)

            case Tag.LocalModify =>
              val instr2 = comp2.asInstanceOf[CC.LocalModify[Local]]
              val store2 = store.deepModify(entry.storeIndex, entry.segmentDepth, instr2)
              innerLoopStep((), step, store2)

            case Tag.LocalUpdate =>
              val instr2 = comp2.asInstanceOf[CC.LocalUpdate[Any, Any, Local]]
              val store2 = store.deepClone(entry.segmentDepth)
              val value = store2.deepUpdateInPlace(entry.storeIndex, entry.segmentDepth, instr2)
              innerLoopStep(value, step, store2)

            case _ => innerLoopComp(comp2, step, store)

        case Tag.Pure =>
          val instr = payload.asInstanceOf[CC.Pure[Any]]
          val payload2 = instr.value
          innerLoopStep(payload2, step, store)

        case Tag.Impure =>
          val instr = payload.asInstanceOf[CC.Impure[Any]]
          val payload2 = instr()
          innerLoopStep(payload2, step, store)

        case Tag.LocalGet =>
          val instr = payload.asInstanceOf[CC.LocalGet]
          val entry = findEntryByPrompt(instr.prompt, stack, hasShadow)
          val local = store.deepGet(entry.storeIndex, entry.segmentDepth)
          innerLoopStep(local, step, store)

        case Tag.LocalGetsEff =>
          val instr = payload.asInstanceOf[CC.LocalGetsEff[Any, Any, Local]]
          val entry = findEntryByPrompt(instr.prompt, stack, hasShadow)
          val local = store.deepGet(entry.storeIndex, entry.segmentDepth)
          val comp2 = instr(local)
          innerLoopComp(comp2, step, store)

        case Tag.LocalPut =>
          val instr = payload.asInstanceOf[CC.LocalPut[Local]]
          val entry = findEntryByPrompt(instr.prompt, stack, hasShadow)
          val store2 = store.deepPut(entry.storeIndex, entry.segmentDepth, instr.local)
          innerLoopStep((), step, store2)

        case Tag.LocalModify =>
          val instr = payload.asInstanceOf[CC.LocalModify[Local]]
          val entry = findEntryByPrompt(instr.prompt, stack, hasShadow)
          val store2 = store.deepModify(entry.storeIndex, entry.segmentDepth, instr)
          innerLoopStep((), step, store2)

        case Tag.LocalUpdate =>
          val instr = payload.asInstanceOf[CC.LocalUpdate[Any, Any, Local]]
          val entry = findEntryByPrompt(instr.prompt, stack, hasShadow)
          val store2 = store.deepClone(entry.segmentDepth)
          val value = store2.deepUpdateInPlace(entry.storeIndex, entry.segmentDepth, instr)
          innerLoopStep(value, step, store2)

        case Tag.Sync =>
          val instr = payload.asInstanceOf[CC.Sync[Any, Any]]
          var result: Any = null
          var throwable: Throwable | Null = null
          try
            result = instr()
          catch
            case e => throwable = e
          if throwable == null then
            val payload2 = if instr.isAttempt then Right(result) else result
            innerLoopStep(payload2, step, store)
          else
            if instr.isAttempt then
              innerLoopStep(Left(throwable), step, store)
            else
              innerLoopStep(Cause(throwable.nn), Step.Throw, store)

        case Tag.Intrinsic | Tag.Unwind =>
          this.savedPayload = payload
          this.savedStep    = step
          this.savedStack   = stack
          this.savedStore   = store
          tag
    else
      this.savedPayload = payload
      this.savedStep    = step
      this.savedStack   = stack
      this.savedStore   = store
      Tag.TickReset + tag


  //-------------------------------------------------------------------
  // Loop Aux
  //-------------------------------------------------------------------


  private final def endOfLoop(completion: Int, payload: Any): Tag =
    currentFiber.doFinalize(completion, payload) match
      case null => Tag.Retire
      case fiber2 => become(fiber2.nn); Tag.Become


  private final def dispatchNotify(tag: Tag): Tag =
    val tag2 = savedStep.tag
    (tag: @switch) match
      case Tag.NotifyOnceVar =>
        val ovar = savedPayload.asInstanceOf[OnceVarImpl]
        this.savedPayload = ovar.theContent
        tag2

      case Tag.NotifyEffectfulVar =>
        val evar = savedPayload.asInstanceOf[EffectfulVarImpl]
        val comp = evar.getNextShot
        this.savedPayload = comp
        comp.tag

      case Tag.NotifyZipper =>
        val fiber = savedPayload.asInstanceOf[FiberImpl]
        this.savedPayload = fiber.getOrMakeZipper
        tag2

      case Tag.NotifyUnit =>
        this.savedPayload = ()
        tag2

      case Tag.NotifyEither =>
        savedPayload.asInstanceOf[Either[Throwable, Any]] match
          case Right(a) =>
            savedPayload = a
            tag2
          case Left(e) =>
            savedPayload = Cause(e)
            savedStep = Step.Throw
            Step.Throw.tag

      case _ => tag


  private final def doUnwind(): Tag =
    val payload = savedPayload
    val step    = savedStep
    val stack   = savedStack
    val store   = savedStore
    //-------------------
    val instr = step.asInstanceOf[Step.Unwind]
    if stack.canPop then
      val (stack2, store2, step2, prompt, frame, local) = OpPush.pop(stack, store)
      val fallthrough = if instr.isPop then step2 else step
      if prompt.isIo then
        (frame.kind.unwrap: @switch) match
          case FrameKind.PLAIN =>
            loopStepRefreshEnv(payload, fallthrough, stack2, store2)

          case FrameKind.GUARD =>
            val payload2 = instr.kind match
              case Step.UnwindKind.Pop    => Snap.Success(payload)
              case Step.UnwindKind.Abort  => Snap.Aborted(payload, instr.prompt.nn)
              case Step.UnwindKind.Cancel => Snap.Cancelled
              case Step.UnwindKind.Throw  => Snap.Failure(payload.asInstanceOf[Cause])
            loopStepRefreshEnv(payload2, step2, stack2, store2)

          case FrameKind.WARP =>
            currentFiber.suspendStep(payload, fallthrough, stack2, store2)
            val warp = currentEnv.currentWarp.nn
            val tried = warp.exitMode match
              case Warp.ExitMode.Cancel => warp.tryGetCancelledBy(currentFiber, currentEnv.isCancellable)
              case Warp.ExitMode.Await => warp.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable)
              case null => impossible //// this is a scoped warp, so it must have ExitMode
            tried match
              case Bits.WaiterSubscribed => ThreadDisowned
              case Bits.WaiterAlreadyCancelled => impossible //// Latch is set
              case Bits.WaiteeAlreadyCompleted =>
                currentFiber.clearSuspension()
                loopStepRefreshEnv(payload, fallthrough, stack2, store2)

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
      else //// isIo
        if instr.isPop then
          val comp2 = prompt.onReturn(payload, local)
          loopComp(comp2, step2, stack2, store2)
        else
          val step3 = if prompt == instr.prompt then step2 else step
          loopStep(payload, step3, stack2, store2)
    else //// canPop
      val completion = instr.kind match
        case Step.UnwindKind.Pop    => Bits.Completion_Success
        case Step.UnwindKind.Cancel => Bits.Completion_Cancelled
        case Step.UnwindKind.Throw  => Bits.Completion_Failure
        case _                      => impossible
      endOfLoop(completion, payload)


  //-------------------------------------------------------------------
  // Intrinsics Aux
  //-------------------------------------------------------------------


  private final def loopStep(value: Any, step: Step, stack: Stack, store: Store): Tag =
    savedPayload = value
    savedStep = step
    savedStack = stack
    savedStore = store
    step.tag

  private final def loopComp(comp: !![?, ?], step: Step, stack: Stack, store: Store): Tag =
    savedPayload = comp
    savedStep = step
    savedStack = stack
    savedStore = store
    comp.tag

  private final def loopStepRefreshEnv(value: Any, step: Step, stack: Stack, store: Store): Tag =
    refreshEnv(stack, store)
    loopStep(value, step, stack, store)

  private final def loopCompRefreshEnv(comp: !![?, ?], step: Step, stack: Stack, store: Store): Tag =
    refreshEnv(stack, store)
    loopComp(comp, step, stack, store)

  private final def loopCancel(stack: Stack, store: Store): Tag =
    loopStep(CancelPayload, Step.Cancel, stack, store)


  //-------------------------------------------------------------------
  // Intrinsics
  //-------------------------------------------------------------------


  final def intrinsicDelimitPut[S](prompt: Prompt, body: AnyComp, local: S): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val location = stack.locatePrompt(prompt)
    val (stack2, store2) = OpPush.pushNested(stack, store, step, prompt, location, local.asLocal, FrameKind.plain)
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicDelimitMod[S](prompt: Prompt, body: AnyComp, fun: S => S): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val location = stack.locatePrompt(prompt)
    val local2 = fun.asInstanceOf[Local => Local](store.deepGet(location))
    val (stack2, store2) = OpPush.pushNested(stack, store, step, prompt, location, local2, FrameKind.plain)
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicAbort(prompt: Prompt, value: Any): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    loopStep(value, Step.abort(prompt), stack, store)


  final def intrinsicShadow[A, U](prompt: Prompt, body: A !! U): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val env2 = currentEnv.copy(shadowMap = currentEnv.shadowMap.push(prompt))
    val (stack2, store2) = OpPush.pushEnv(stack, store, step, env2)
    this.currentEnv = env2
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicResume[A, B, S, U](cont0: Continuation[A, B, S, U], value: A): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val cont = cont0.asImpl
    val (step2, stack2, store2) = OpSplit.merge(
      stepHi  = cont.step,
      stackHi = cont.stack,
      storeHi = cont.store,
      stepLo  = step,
      stackLo = stack,
      storeLo = store,
    )
    loopStepRefreshEnv(value, step2, stack2, store2)


  final def intrinsicResumePut[A, B, S, U](cont0: Continuation[A, B, S, U], value: A, local: S): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val cont = cont0.asImpl
    val (step2, stack2, store2) = OpSplit.merge(
      stepHi  = cont.step,
      stackHi = cont.stack,
      storeHi = cont.store.deepPutIfNotVoid(cont.location, local.asLocal),
      stepLo  = step,
      stackLo = stack,
      storeLo = store,
    )
    loopStepRefreshEnv(value, step2, stack2, store2)


  final def intrinsicCapture[A, B, C, S, U, V](prompt: Prompt, fun: Continuation[A, B, S, U] => C !! V, truncate: Boolean): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val location = stack.locatePrompt(prompt)
    val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(stack, store, location, truncate)
    //@#@THOV only the shallow part of location2 is used, and only in `resumePut`
    //// `invalid` is safe bcoz `resumePut` can't be called on truncated continuation
    val location2 = if truncate then Location.Deep.invalid else stackHi.locatePrompt(prompt)
    val cont = new ContImpl(stackHi, storeHi, step, location2)
    val comp2 = fun(cont.cast[A, B, S, U])
    loopCompRefreshEnv(comp2, stepMid, stackLo, storeLo)


  final def intrinsicCaptureGet[A, B, C, S, U, V](prompt: Prompt, fun: (Continuation[A, B, S, U], S) => C !! V, truncate: Boolean): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val location = stack.locatePrompt(prompt)
    val local = store.deepGet(location)
    val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(stack, store, location, truncate)
    //@#@THOV only the shallow part of location2 is used, and only in `resumePut`
    //// `invalid` is safe bcoz `resumePut` can't be called on truncated continuation
    val location2 = if truncate then Location.Deep.invalid else stackHi.locatePrompt(prompt)
    val cont = new ContImpl(stackHi, storeHi, step, location2)
    val comp2 = fun(cont.cast[A, B, S, U], local.asInstanceOf[S])
    loopCompRefreshEnv(comp2, stepMid, stackLo, storeLo)


  final def intrinsicReinterpret[A, U, V](body: A !! (U & V)): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    //@#@TODO shadow map
    loopComp(body, step, stack, store)


  final def intrinsicZipPar[A, B, C, U](lhs: A !! U, rhs: B !! U, fun: (A, B) => C): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    //@#@TODO Too conservative? Should check for `features.isParallel` at `mark`, instead of at stack top
    if stack.accumFeatures.isParallel && currentEnv.isParallelismRequested then
      val fiberLeft = currentFiber.createChild(Bits.ZipPar_Left)
      val fiberRight = currentFiber.createChild(Bits.ZipPar_Right)
      if currentFiber.tryStartRace(fiberLeft, fiberRight, currentEnv.isCancellable) then
        val stack2 = stack.lazyFork
        val (storeDown, storeLeft, storeRight) = OpCascaded.fork2(stack, store, stack2)
        currentFiber.suspendForRace(fun, step, stack, storeDown)
        fiberRight.suspendComp(rhs, Step.Pop, stack2, storeRight)
        fiberRight.resume()
        becomeWithSameEnv(fiberLeft)
        loopComp(lhs, Step.Pop, stack2, storeLeft)
      else
        //// Must have been cancelled meanwhile
        loopCancel(stack, store)
    else
      //// Fallback to sequential
      val comp2 = lhs.zipWith(rhs)(fun)
      loopComp(comp2, step, stack, store)


  final def intrinsicOrPar[A, U](lhs: A !! U, rhs: A !! U): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    if stack.accumFeatures.isParallel && currentEnv.isParallelismRequested then
      val fiberLeft = currentFiber.createChild(Bits.OrPar_Left)
      val fiberRight = currentFiber.createChild(Bits.OrPar_Right)
      if currentFiber.tryStartRace(fiberLeft, fiberRight, currentEnv.isCancellable) then
        val stack2 = stack.lazyFork
        val (storeDown, storeLeft, storeRight) = OpCascaded.fork2(stack, store, stack2)
        currentFiber.suspendForRace(null, step, stack, storeDown)
        fiberRight.suspendComp(rhs, Step.Pop, stack2, storeRight)
        fiberRight.resume()
        becomeWithSameEnv(fiberLeft)
        loopComp(lhs, Step.Pop, stack2, storeLeft)
      else
        //// Must have been cancelled meanwhile
        loopCancel(stack, store)
    else
      //// Fallback to sequential
      val comp2 = lhs ||! rhs
      loopComp(comp2, step, stack, store)


  final def intrinsicOrSeq[A, U](lhs: A !! U, rhsFun: () => A !! U): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val fiberLeft = currentFiber.createChild(Bits.OrSeq)
    if currentFiber.tryStartRaceOfOne(fiberLeft, currentEnv.isCancellable) then
      val stack2 = stack.lazyFork
      val (storeDown, storeFork) = OpCascaded.fork1(stack, store, stack2)
      currentFiber.suspendForRace(rhsFun, step, stack, storeDown)
      becomeWithSameEnv(fiberLeft)
      loopComp(lhs, Step.Pop, stack2, storeFork)
    else
      //// Must have been cancelled meanwhile
      loopCancel(stack, store)


  final def intrinsicHandle(body: AnyComp, prompt: Prompt, initial: Any): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    // for sig <- prompt.signatures do
    //   if stack.containsSignature(sig) then
    //     panic(s"Unsupported feature: shadowing effect ${sig}.")
    val (stack2, store2) = OpPush.pushBase(stack, store, step, prompt, initial.asLocal)
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicSnap[A, U](body: A !! U): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val (stack2, store2) = OpPush.pushGuard(stack, store, step)
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicUnsnap[A, U](snap: Snap[A]): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    //@#@TODO forbid uncancelling, it wouldnt work correctly anyway
    (snap: @unchecked) match
      case Snap.Success(payload2)         => loopStep(payload2, step, stack, store)
      case Snap.Failure(payload2)         => loopStep(payload2, Step.Throw, stack, store)
      case Snap.Aborted(payload2, prompt) => loopStep(payload2, Step.abort(prompt), stack, store)
      case Snap.Cancelled =>
        //@#@THOV It should be harmless to self-cancel a fiber, even when it's uncancellable?
        currentFiber.cancelBySelf()
        loopCancel(stack, store)


  final def intrinsicEnvAsk[A](fun: Env => A): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val value = fun(currentEnv)
    loopStep(value, step, stack, store)


  final def intrinsicEnvMod[A, U](fun: Env => Env, body: A !! U): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val env2 = fun(currentEnv)
    if currentEnv == env2 then
      loopComp(body, step, stack, store)
    else
      val (stack2, store2) = OpPush.pushEnv(stack, store, step, env2)
      this.currentEnv = env2
      loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicForkFiber[A, U](warp0: Warp | Null, comp: A !! U, name: String, callback: (Zipper.Untyped => Unit) | Null = null): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val warp = if warp0 != null then warp0.nn.asImpl else currentEnv.currentWarp.nn
    val stackFork = stack.lazyFork
    val (storeDown, storeFork) = OpCascaded.fork1(stack, store, stackFork)
    val child = FiberImpl.createExplicit(stackFork, warp, name, callback)
    child.suspendComp(comp, Step.Pop, stackFork, storeFork)
    if warp.tryAddFiber(child) then
      child.resume()
      loopStep(child, step, stack, storeDown)
    else
      child.suspendAsCancelled()
      loopStep(child, step, stack, store)


  final def intrinsicAwaitFiber[A, U](fiber: Fiber.Untyped, isCancel: Boolean, isVoid: Boolean): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val waitee = fiber.asImpl
    if currentFiber != waitee then
      val notifyTag = if isVoid then Tag.NotifyUnit else Tag.NotifyZipper
      currentFiber.suspend(notifyTag, waitee, step, stack, store)
      val tried =
        if isCancel
        then waitee.tryGetCancelledBy(currentFiber, currentEnv.isCancellable)
        else waitee.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable)
      tried match
        case Bits.WaiterSubscribed => ThreadDisowned
        case Bits.WaiterAlreadyCancelled =>
          currentFiber.clearSuspension()
          loopCancel(stack, store)
        case Bits.WaiteeAlreadyCompleted =>
          currentFiber.clearSuspension()
          val payload2 = if isVoid then () else waitee.getOrMakeZipper
          loopStep(payload2, step, stack, store)
    else
      //// Ignoring `isCancellable` bcoz cancelling is by-self
      if isCancel then
        currentFiber.cancelBySelf()
        loopCancel(stack, store)
      else
        val zombie = new Blocker.Zombie(currentFiber)
        currentFiber.suspendStep(null, step, stack, store)
        if currentFiber.tryGetBlocked(zombie, currentEnv.isCancellable) then
          ThreadDisowned
        else
          currentFiber.clearSuspension()
          loopCancel(stack, store)


  final def intrinsicCurrentFiber(): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    loopStep(currentFiber, step, stack, store)


  final def intrinsicSpawnWarp[A, U](exitMode: Warp.ExitMode, body: A !! (U & Warp), name: String): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val warp = new WarpImpl(currentFiber, currentEnv.currentWarp, name, exitMode)
    val env2 = currentEnv.copy(currentWarp = warp)
    val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.warp)
    this.currentEnv = env2
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicAwaitWarp(warp0: Warp, isCancel: Boolean): Tag =
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
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAsync[A](callback: (Either[Throwable, A] => Unit) => Unit, isAttempt: Boolean): Tag =
    currentFiber.suspend(if isAttempt then savedStep.tag else Tag.NotifyEither, null, savedStep, savedStack, savedStore)
    callback(currentFiber)
    ThreadDisowned


  final def intrinsicBlocking[A, B](thunk: () => A, isAttempt: Boolean): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val blocker = new Blocker.Interruptible(currentFiber, thunk, isAttempt)
    currentFiber.suspendStep(null, step, stack, store)
    if currentFiber.tryGetBlocked(blocker, currentEnv.isCancellable) then
      blocker.block()
      ThreadDisowned
    else
      currentFiber.clearSuspension()
      loopCancel(stack, store)


  final def intrinsicSleep(length: Long, unit: TimeUnit = TimeUnit.MILLISECONDS): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val blocker = new Blocker.Sleeper(currentFiber)
    currentFiber.suspendStep((), step, stack, store)
    if currentFiber.tryGetBlocked(blocker, currentEnv.isCancellable) then
      blocker.sleep(length, unit)
      ThreadDisowned
    else
      currentFiber.clearSuspension()
      loopCancel(stack, store)


  final def intrinsicSuppress[A, U](body: A !! U, delta: Int): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val n1 = currentEnv.suppressions
    val n2 = 0.max(n1 + delta)
    if n1 == n2 then
      loopComp(body, step, stack, store)
    else
      val env2 = currentEnv.copy(suppressions = n2)
      val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.suppress)
      this.currentEnv = env2
      if cancellationCheck() then
        loopCancel(stack2, store2)
      else
        loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicExecOn[A, U](exec: Executor, body: A !! U): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    if currentEnv.executor == exec then
      loopComp(body, step, stack, store)
    else
      val env2 = currentEnv.copy(executor = exec)
      val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.exec)
      currentFiber.suspendComp(body, Step.Pop, stack2, store2)
      currentFiber.resume()
      ThreadDisowned


  final def intrinsicYield: Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspendStep((), step, stack, store)
    Tag.Yield


  final def intrinsicAwaitOnceVar[A](ovar0: OnceVar.Get[A]): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val ovar = ovar0.asImpl
    val value = ovar.theContent
    if OnceVarImpl.Empty != value then
      loopStep(value, step, stack, store)
    else
      currentFiber.suspend(Tag.NotifyOnceVar, ovar, step, stack, store)
      ovar.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable) match
        case Bits.WaiterSubscribed => ThreadDisowned
        case Bits.WaiterAlreadyCancelled =>
          currentFiber.clearSuspension()
          loopCancel(stack, store)
        case Bits.WaiteeAlreadyCompleted =>
          currentFiber.clearSuspension()
          val value = ovar.theContent
          loopStep(value, step, stack, store)


  final def intrinsicAwaitEffectfulVar[A, U <: IO](evar0: EffectfulVar.Get[A, U]): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    val evar = evar0.asImpl
    if evar.isReady then
      loopComp(evar.getNextShot, step, stack, store)
    else
      currentFiber.suspend(Tag.NotifyEffectfulVar, evar, step, stack, store)
      val (code, comp2) = evar.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable)
      code match
        case Bits.WaiterSubscribed => ThreadDisowned
        case Bits.WaiterAlreadyCancelled =>
          currentFiber.clearSuspension()
          loopCancel(stack, store)
        case Bits.WaiteeAlreadyCompleted =>
          currentFiber.clearSuspension()
          loopComp(comp2.nn, step, stack, store)


  final def intrinsicAwaitCountDownLatch(latch: CountDownLatch): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspendStep((), step, stack, store)
    latch.asImpl.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAwaitCyclicBarrier(barrier: CyclicBarrier): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspendStep((), step, stack, store)
    barrier.asImpl.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAcquireReentrantLock(lock: ReentrantLock): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspendStep((), step, stack, store)
    lock.asImpl.tryGetAcquiredBy(currentFiber, currentEnv.isCancellable) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAcquireMutex(mutex: Mutex): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspendStep((), step, stack, store)
    mutex.asImpl.tryGetAcquiredBy(currentFiber, currentEnv.isCancellable) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAcquireSemaphore(semaphore: Semaphore, count: Long): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspend(Tag.NotifyUnit, count, step, stack, store)
    semaphore.asImpl.tryGetAcquiredBy(currentFiber, currentEnv.isCancellable, count) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicGetChannel[A](channel: Channel.Get[A]): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspendStep(null, step, stack, store)
    val (code, value) = channel.asImpl.tryGetBy(currentFiber, currentEnv.isCancellable)
    code match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        loopStep(value, step, stack, store)


  final def intrinsicPutChannel[A](channel: Channel.Put[A], value: A): Tag =
    val step = savedStep
    val stack = savedStack
    val store = savedStore
    //-------------------
    currentFiber.suspend(Tag.NotifyUnit, value, step, stack, store)
    channel.asImpl.tryPutBy(currentFiber, currentEnv.isCancellable) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        currentFiber.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        currentFiber.clearSuspension()
        loopStep((), step, stack, store)


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


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


  private final def clearSaved(): Unit =
    this.savedPayload = null
    this.savedStep = null.asInstanceOf[Step]
    this.savedStack = null.asInstanceOf[Stack]
    this.savedStore = null.asInstanceOf[Store]


  private final def findEntryBySignature(sig: Signature, stack: Stack, hasShadow: Boolean): Entry =
    if !hasShadow then
      stack.findEntryBySignature(sig)
    else
      stack.findEntryBySignatureWithShadow(sig, currentEnv.shadowMap.get(sig))


  private final def findEntryByPrompt(prompt: Prompt, stack: Stack, hasShadow: Boolean): Entry =
    if !hasShadow then
      stack.findEntryByPrompt(prompt)
    else
      stack.findEntryByPromptWithShadow(prompt, currentEnv.shadowMap.get(prompt))
