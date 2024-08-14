package turbolift.internals.engine
import scala.annotation.{tailrec, switch}
import turbolift.{Computation, Signature}
import turbolift.io.{Fiber, Zipper, Warp, OnceVar, Snap, Outcome, Cause, Exceptions}
import turbolift.internals.primitives.{Tags, ComputationCases => CC}
import turbolift.internals.executor.Executor
import turbolift.internals.engine.stacked.{StepCases => SC, Step, Stack, Store, Local, Prompt, FrameKind, OpPush, OpSplit, OpCascaded}
import turbolift.internals.engine.concurrent.{Bits, Blocker, FiberImpl, WarpImpl, OnceVarImpl}
import Halt.{Retire => ThreadDisowned}
import Local.Syntax._
import Prompt.Syntax._
import Cause.{Cancelled => CancelPayload}
import Misc._


private sealed abstract class MainLoop0 extends Runnable:
  protected var currentFiber: FiberImpl = null.asInstanceOf[FiberImpl]
  protected var currentEnv: Env = null.asInstanceOf[Env]
  protected var currentTickLow: Int = 0
  protected var currentTickHigh: Int = 0


private[internals] abstract class MainLoop extends MainLoop0:
  protected var savedTag: Int = 0
  protected var savedPayload: Any = null
  protected var savedStep: Step = null.asInstanceOf[Step]
  protected var savedStack: Stack = null.asInstanceOf[Stack]
  protected var savedStore: Store = null.asInstanceOf[Store]
  protected[this] val pad1 = 0L
  protected[this] val pad2 = 0


  def this(fiber: FiberImpl) = { this(); become(fiber) }


  final def runCurrent(): Halt =
    currentFiber.theOwnership = Bits.Ownership_Self
    currentTickLow = currentEnv.tickLow
    currentTickHigh = currentEnv.tickHigh
    if cancellationCheck() then
      currentFiber.suspendAsCancelled()
    outerLoop()

  
  private final def bounce(tag: Int, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
    savedTag     = tag
    savedPayload = payload
    savedStep    = step
    savedStack   = stack
    savedStore   = store
    Halt.Bounce


  private final def unbounce(): Unit =
    savedPayload = null
    savedStep = null.asInstanceOf[Step]
    savedStack = null.asInstanceOf[Stack]
    savedStore = null.asInstanceOf[Store]


  //-------------------------------------------------------------------
  // Outer Loop
  //-------------------------------------------------------------------


  @tailrec private final def outerLoop(): Halt =
    val result =
      val tag     = currentFiber.suspendedTag
      val payload = currentFiber.suspendedPayload.nn
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

    if currentTickLow > 0 then
      currentTickLow -= 1
      (tag: @switch) match
        case Tags.MapFlat | Tags.MapPure =>
          val instr = payload.asInstanceOf[CC.Map[Any, Any]]
          val comp2 = instr.comp
          val tag2 = tag + Tags.Step_MoreFlat - Tags.MapFlat
          val step2 = SC.More(tag2, instr.fun, step)
          loopComp(comp2, step2, stack, store)

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
          val comp2 = instr.op(prompt)
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
              val (value, store2) = store.updateDeep(location, instr2.fun)
              loopStep(value, step, stack, store2)

            case _ => loopComp(comp2, step, stack, store)

        case Tags.Pure =>
          val instr = payload.asInstanceOf[CC.Pure[Any]]
          val payload2 = instr.value
          loopStep(payload2, step, stack, store)

        case Tags.Impure =>
          val instr = payload.asInstanceOf[CC.Impure[Any, Any]]
          val payload2 = instr.thunk()
          loopStep(payload2, step, stack, store)

        case _ =>
          loopMore(tag, payload, step, stack, store) match
            case Halt.Bounce =>
              val tag2     = savedTag
              val payload2 = savedPayload
              val step2    = savedStep
              val stack2   = savedStack
              val store2   = savedStore
              unbounce()
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


  private def loopMore(tag: Int, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
    inline def loopStep(payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
      bounce(step.tag, payload, step, stack, store)

    inline def loopComp(comp: Computation[?, ?], step: Step, stack: Stack, store: Store): Halt.Loop2nd =
      bounce(comp.tag, comp, step, stack, store)

    inline def loopCancel(stack: Stack, store: Store): Halt.Loop2nd =
      bounce(Tags.Step_Unwind, CancelPayload, Step.Cancel, stack, store)

    inline def innerLoop(tag: Int, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop2nd =
      bounce(tag, payload, step, stack, store)

    (tag: @switch) match
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
        val (value, store2) = store.updateDeep(location, instr.fun)
        loopStep(value, step, stack, store2)

      case Tags.Delimit =>
        val instr = payload.asInstanceOf[CC.Delimit[Any, Local, Any]]
        val location = stack.locatePrompt(instr.prompt)
        val local =
          if instr.fun == null
          then instr.local
          else instr.fun(store.getDeep(location))
        val (stack2, store2) = OpPush.pushNested(stack, store, step, instr.prompt, location, local, FrameKind.plain)
        loopComp(instr.body, SC.Pop, stack2, store2)

      case Tags.Abort =>
        val instr = payload.asInstanceOf[CC.Abort[Any, Any]]
        val step2 = instr.prompt.unwind
        val payload2 = instr.value
        loopStep(payload2, step2, stack, store)

      case Tags.Resume =>
        val instr = payload.asInstanceOf[CC.Resume[Any, Any, Local, Any]]
        val cont = instr.cont.asImpl
        val (stack2, store2) = OpSplit.merge(
          stackHi = cont.stack,
          storeHi = cont.store.setDeepIfNotVoid(cont.location, instr.local),
          stepMid = step,
          stackLo = stack,
          storeLo = store,
        )
        refreshEnv(stack2, store2)
        loopStep(instr.value, cont.step, stack2, store2)

      case Tags.Capture =>
        val instr = payload.asInstanceOf[CC.Capture[Any, Any, Any, Any]]
        val location = stack.locatePrompt(instr.prompt)
        val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(stack, store, location)
        //@#@THOV only the shallow paty of location2 is used
        val location2 = stackHi.locatePrompt(instr.prompt)
        val cont = new ContImpl(stackHi, storeHi, step, location2)
        val comp2 = (instr.fun: @unchecked) match
          case f: Function1[Any, AnyComp] => f(cont)
          case f: Function2[Any, Any, AnyComp] =>
            val local = storeHi.getDeep(location2)
            f(cont, local)
        refreshEnv(stackLo, storeLo)
        loopComp(comp2, stepMid, stackLo, storeLo)

      case Tags.Step_Bridge =>
        val (stack2, store2, step2) = OpPush.drop(stack, store)
        refreshEnv(stack2, store2)
        loopStep(payload, step2, stack2, store2)

      case Tags.ZipPar =>
        val instr = payload.asInstanceOf[CC.ZipPar[Any, Any, Any, Any]]
        //@#@TODO Too conservative? Should check for `features.isParallel` at `mark`, instead of at stack top
        if stack.accumFeatures.isParallel && currentEnv.isParallelismRequested then
          val fiberLeft = currentFiber.createChild(Bits.ZipPar_Left)
          val fiberRight = currentFiber.createChild(Bits.ZipPar_Right)
          if currentFiber.tryStartRace(fiberLeft, fiberRight, currentEnv.isCancellable) then
            val (storeTmp, storeLeft) = OpCascaded.fork(stack, store)
            val (storeDown, storeRight) = OpCascaded.fork(stack, storeTmp)
            currentFiber.suspendForRace(instr.fun, step, stack, storeDown)
            val stack2 = stack.makeFork
            fiberRight.suspend(instr.rhs.tag, instr.rhs, SC.Pop, stack2, storeRight)
            fiberRight.resume()
            becomeWithSameEnv(fiberLeft)
            innerLoop(instr.lhs.tag, instr.lhs, SC.Pop, stack2, storeLeft)
          else
            //// Must have been cancelled meanwhile
            loopCancel(stack, store)
        else
          //// Fallback to sequential
          val comp2 = instr.lhs.zipWith(instr.rhs)(instr.fun)
          loopComp(comp2, step, stack, store)

      case Tags.OrPar =>
        val instr = payload.asInstanceOf[CC.OrPar[Any, Any]]
        if stack.accumFeatures.isParallel && currentEnv.isParallelismRequested then
          val fiberLeft = currentFiber.createChild(Bits.OrPar_Left)
          val fiberRight = currentFiber.createChild(Bits.OrPar_Right)
          if currentFiber.tryStartRace(fiberLeft, fiberRight, currentEnv.isCancellable) then
            val (storeTmp, storeLeft) = OpCascaded.fork(stack, store)
            val (storeDown, storeRight) = OpCascaded.fork(stack, storeTmp)
            currentFiber.suspendForRace(null, step, stack, storeDown)
            val stack2 = stack.makeFork
            fiberRight.suspend(instr.rhs.tag, instr.rhs, SC.Pop, stack2, storeRight)
            fiberRight.resume()
            becomeWithSameEnv(fiberLeft)
            innerLoop(instr.lhs.tag, instr.lhs, SC.Pop, stack2, storeLeft)
          else
            //// Must have been cancelled meanwhile
            loopCancel(stack, store)
        else
          //// Fallback to sequential
          val comp2 = CC.OrSeq(instr.lhs, () => instr.rhs)
          loopComp(comp2, step, stack, store)

      case Tags.OrSeq =>
        val instr = payload.asInstanceOf[CC.OrSeq[Any, Any]]
        val fiberLeft = currentFiber.createChild(Bits.OrSeq)
        if currentFiber.tryStartRaceOfOne(fiberLeft, currentEnv.isCancellable) then
          val (storeDown, storeLeft) = OpCascaded.fork(stack, store)
          currentFiber.suspendForRace(instr.rhsFun, step, stack, storeDown)
          val stack2 = stack.makeFork
          becomeWithSameEnv(fiberLeft)
          innerLoop(instr.lhs.tag, instr.lhs, SC.Pop, stack2, storeLeft)
        else
          //// Must have been cancelled meanwhile
          loopCancel(stack, store)

      case Tags.Handle =>
        val instr = payload.asInstanceOf[CC.Handle[Any, Any, [_] =>> Any, [_] =>> Any, Any, Any]]
        val prompt = instr.handler.interpreter.untyped
        for sig <- prompt.signatures do
          if stack.containsSignature(sig) then
            panic(s"Unsupported feature: shadowing effect ${sig}.")
        val comp2 = prompt.onInitial
        val step2 = new SC.Push(instr.body, prompt, step)
        loopComp(comp2, step2, stack, store)

      case Tags.Step_Push =>
        val instr = step.asInstanceOf[SC.Push]
        val step2 = instr.next
        val (stack2, store2) = OpPush.pushBase(stack, store, step2, instr.prompt, payload.asLocal)
        val comp2 = instr.body
        loopComp(comp2, SC.Pop, stack2, store2)

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
                currentFiber.suspend(fallthrough.tag, payload, fallthrough, stack2, store2)
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
                currentFiber.suspend(fallthrough.tag, payload, fallthrough, stack2, store2)
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

      case Tags.DoIO =>
        val instr = payload.asInstanceOf[CC.DoIO[Any, Any]]
        var result: Any = null
        var throwable: Throwable | Null = null
        try
          result = instr.thunk()
        catch
          case e => throwable = e
        if throwable == null then
          val payload2 = if instr.isAttempt then Right(result) else result
          loopStep(payload2, step, stack, store)
        else
          if instr.isAttempt then
            val payload2 = Left(throwable)
            loopStep(payload2, step, stack, store)
          else
            val step2 = Step.Throw
            val payload2 = Cause(throwable.nn)
            loopStep(payload2, step2, stack, store)

      case Tags.DoSnap =>
        val instr = payload.asInstanceOf[CC.DoSnap[Any, Any]]
        val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, Local.void, FrameKind.guard)
        loopComp(instr.body, SC.Pop, stack2, store2)

      case Tags.Unsnap =>
        val instr = payload.asInstanceOf[CC.Unsnap[Any, Any]]
        //@#@TODO forbid uncancelling, it wouldnt work correctly anyway
        instr.snap match
          case Snap.Success(payload2) =>
            loopStep(payload2, step, stack, store)
          case Snap.Failure(payload2) =>
            val step2 = Step.Throw
            loopStep(payload2, step2, stack, store)
          case theAborted: Snap.Aborted =>
            val payload2 = theAborted.value
            val step2 = theAborted.prompt.unwind
            loopStep(payload2, step2, stack, store)
          case Snap.Cancelled =>
            //@#@THOV It should be harmless to self-cancel a fiber, even when it's uncancellable?
            currentFiber.cancelBySelf()
            loopCancel(stack, store)

      case Tags.EnvAsk =>
        val instr = payload.asInstanceOf[CC.EnvAsk[Any]]
        val value = instr.fun(currentEnv)
        loopStep(value, step, stack, store)

      case Tags.EnvMod =>
        val instr = payload.asInstanceOf[CC.EnvMod[Any, Any]]
        val env2 = instr.fun(currentEnv)
        if currentEnv == env2 then
          loopComp(instr.body, step, stack, store)
        else
          val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.plain)
          this.currentEnv = env2
          loopComp(instr.body, SC.Pop, stack2, store2)

      case Tags.AwaitOnceVar =>
        val instr = payload.asInstanceOf[CC.AwaitOnceVar[Any]]
        val ovar = instr.ovar.asImpl
        val value = ovar.theContent
        if OnceVarImpl.Empty != value then
          loopStep(value, step, stack, store)
        else
          currentFiber.suspend(Tags.NotifyOnceVar, ovar, step, stack, store)
          ovar.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable) match
            case Bits.WaiterSubscribed => ThreadDisowned
            case Bits.WaiterAlreadyCancelled =>
              currentFiber.clearSuspension()
              loopCancel(stack, store)
            case Bits.WaiteeAlreadyCompleted =>
              currentFiber.clearSuspension()
              val value = ovar.theContent
              loopStep(value, step, stack, store)

      case Tags.NotifyOnceVar =>
        val ovar = payload.asInstanceOf[OnceVarImpl]
        val value = ovar.theContent
        loopStep(value, step, stack, store)

      case Tags.ForkFiber =>
        val instr = payload.asInstanceOf[CC.ForkFiber[Any, Any]]
        val warp = if instr.warp != null then instr.warp.nn.asImpl else currentEnv.currentWarp.nn
        val (storeDown, storeFork) = OpCascaded.fork(stack, store)
        val stackFork = stack.makeFork
        val child = FiberImpl.createExplicit(warp, instr.name, instr.callback)
        child.suspend(instr.comp.tag, instr.comp, SC.Pop, stackFork, storeFork)
        if warp.tryAddFiber(child) then
          child.resume()
          loopStep(child, step, stack, storeDown)
        else
          child.suspendAsCancelled()
          loopStep(child, step, stack, store)

      case Tags.AwaitFiber =>
        val instr = payload.asInstanceOf[CC.AwaitFiber[Any, Any]]
        val waitee = instr.fiber.asImpl
        if currentFiber != waitee then
          val tag2 = if instr.isVoid then Tags.NotifyFiberVoid else Tags.NotifyFiber
          currentFiber.suspend(tag2, waitee, step, stack, store)
          val tried =
            if instr.isCancel
            then waitee.tryGetCancelledBy(currentFiber, currentEnv.isCancellable)
            else waitee.tryGetAwaitedBy(currentFiber, currentEnv.isCancellable)
          tried match
            case Bits.WaiterSubscribed => ThreadDisowned
            case Bits.WaiterAlreadyCancelled =>
              currentFiber.clearSuspension()
              loopCancel(stack, store)
            case Bits.WaiteeAlreadyCompleted =>
              currentFiber.clearSuspension()
              val payload2 = if instr.isVoid then () else waitee.getOrMakeZipper
              loopStep(payload2, step, stack, store)
        else
          //// Ignoring `isCancellable` bcoz cancelling is by-self
          if instr.isCancel then
            currentFiber.cancelBySelf()
            loopCancel(stack, store)
          else
            val zombie = new Blocker.Zombie(currentFiber)
            currentFiber.suspend(step.tag, zombie, step, stack, store)
            if currentFiber.tryGetBlocked(currentEnv.isCancellable) then
              ThreadDisowned
            else
              currentFiber.clearSuspension()
              loopCancel(stack, store)

      case Tags.NotifyFiber =>
        val waitee = payload.asInstanceOf[FiberImpl]
        val zipper = waitee.getOrMakeZipper
        loopStep(zipper, step, stack, store)

      case Tags.NotifyFiberVoid =>
        loopStep((), step, stack, store)

      case Tags.CurrentFiber =>
        loopStep(currentFiber, step, stack, store)

      case Tags.SpawnWarp =>
        val instr = payload.asInstanceOf[CC.SpawnWarp[Any, Any]]
        val warp = new WarpImpl(currentFiber, currentEnv.currentWarp, instr.name, instr.exitMode)
        val env2 = currentEnv.copy(currentWarp = warp)
        val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.warp)
        this.currentEnv = env2
        loopComp(instr.body, SC.Pop, stack2, store2)

      case Tags.AwaitWarp =>
        val instr = payload.asInstanceOf[CC.AwaitWarp]
        val warp = instr.warp.asImpl
        currentFiber.suspend(step.tag, (), step, stack, store)
        val tried =
          if instr.isCancel
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

      case Tags.Blocking =>
        val instr = payload.asInstanceOf[CC.Blocking[Any, Any]]
        val blocker = new Blocker.Interruptible(currentFiber, instr.thunk, instr.isAttempt)
        currentFiber.suspend(Tags.NotifyBlocker, blocker, step, stack, store)
        if currentFiber.tryGetBlocked(currentEnv.isCancellable) then
          blocker.block()
          ThreadDisowned
        else
          currentFiber.clearSuspension()
          loopCancel(stack, store)

      case Tags.Sleep =>
        val instr = payload.asInstanceOf[CC.Sleep]
        val blocker = new Blocker.Sleeper(currentFiber)
        currentFiber.suspend(Tags.NotifyBlocker, blocker, step, stack, store)
        if currentFiber.tryGetBlocked(currentEnv.isCancellable) then
          blocker.sleep(instr.length, instr.unit)
          ThreadDisowned
        else
          currentFiber.clearSuspension()
          loopCancel(stack, store)

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
            case Bits.Completion_Success =>
              val payload2 = blocker.getResult
              loopStep(payload2, step, stack, store)
            case Bits.Completion_Cancelled =>
              currentFiber.cancelBySelf()
              loopCancel(stack, store)
            case Bits.Completion_Failure =>
              val step2 = Step.Throw
              val payload2 = Cause(blocker.getThrowable)
              loopStep(payload2, step2, stack, store)

      case Tags.Suppress =>
        val instr = payload.asInstanceOf[CC.Suppress[Any, Any]]
        val n1 = currentEnv.suppressions
        val n2 = 0.max(n1 + instr.delta)
        if n1 == n2 then
          loopComp(instr.body, step, stack, store)
        else
          val env2 = currentEnv.copy(suppressions = n2)
          val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.suppress)
          this.currentEnv = env2
          if cancellationCheck() then
            loopCancel(stack2, store2)
          else
            loopComp(instr.body, SC.Pop, stack2, store2)

      case Tags.ExecOn =>
        val instr = payload.asInstanceOf[CC.ExecOn[Any, Any]]
        if currentEnv.executor == instr.exec then
          loopComp(instr.body, step, stack, store)
        else
          val env2 = currentEnv.copy(executor = instr.exec)
          val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.exec)
          currentFiber.suspend(instr.body.tag, instr.body, SC.Pop, stack2, store2)
          currentFiber.resume()
          ThreadDisowned

      case Tags.Yield =>
        currentFiber.suspend(step.tag, (), step, stack, store)
        Halt.Yield


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
