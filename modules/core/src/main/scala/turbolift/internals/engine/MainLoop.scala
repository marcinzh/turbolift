package turbolift.internals.engine
import scala.annotation.{tailrec, switch}
import turbolift.{Computation, Signature}
import turbolift.io.{Fiber, Zipper, Warp, OnceVar, Snap, Outcome, Cause, Exceptions}
import turbolift.internals.primitives.{Tags, ComputationCases => CC}
import turbolift.internals.executor.Executor
import turbolift.internals.engine.{StepCases => SC}
import Cause.{Cancelled => CancelPayload}
import MainLoop.LoopMore


private final class MainLoop:
  private var currentFiber: FiberImpl = null.asInstanceOf[FiberImpl]
  private var currentEnv: Env = null.asInstanceOf[Env]
  private var currentTickLow: Int = 0
  private var currentTickHigh: Int = 0
  protected[this] val pad1, pad2, pad3, pad4 = 0L


  def run(): Halt =
    currentFiber.theOwnership = Bits.Ownership_Self
    currentTickLow = currentEnv.tickLow
    currentTickHigh = currentEnv.tickHigh
    if currentFiber.cancellationCheck() then
      currentFiber.suspendAsCancelled()
    outerLoop()


  //-------------------------------------------------------------------
  // Outer Loop
  //-------------------------------------------------------------------


  @tailrec private def outerLoop(): Halt =
    val result =
      val currentTag     = currentFiber.suspendedTag
      val currentPayload = currentFiber.suspendedPayload.nn
      val currentStep    = currentFiber.suspendedStep.nn
      val currentStack   = currentFiber.suspendedStack.nn
      val currentStore   = currentFiber.suspendedStore.nn
      currentFiber.clearSuspension()
      try
        innerLoop(
          tag      = currentTag,
          payload  = currentPayload,
          step     = currentStep,
          stack    = currentStack,
          store    = currentStore,
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
  // Inner Loop
  //-------------------------------------------------------------------


  @tailrec private def innerLoop(tag: Byte, payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop =
    inline def loopStep(payload: Any, step: Step, stack: Stack, store: Store): Halt.Loop =
      innerLoop(step.tag, payload, step, stack, store)

    inline def loopComp(comp: Computation[?, ?], step: Step, stack: Stack, store: Store): Halt.Loop =
      innerLoop(comp.tag, comp, step, stack, store)

    inline def loopCancel(stack: Stack, store: Store): Halt.Loop =
      innerLoop(Tags.Step_Unwind, CancelPayload, Step.Cancel, stack, store)

    if currentTickLow > 0 then
      currentTickLow -= 1
      (tag: @switch) match
        case Tags.MapFlat | Tags.MapPure =>
          val instr = payload.asInstanceOf[CC.Map[Any, Any]]
          val comp2 = instr.comp
          val tag2 = (tag + (Tags.Step_MoreFlat - Tags.MapFlat)).toByte
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
          val comp2 = instr.op(prompt).asInstanceOf[AnyComp]
          (comp2.tag: @switch) match
            case Tags.LocalGet =>
              val instr = comp2.asInstanceOf[CC.LocalGet]
              val local = store.getDeep(location)
              loopStep(local, step, stack, store)

            case Tags.LocalPut =>
              val instr = comp2.asInstanceOf[CC.LocalPut[Any]]
              val store2 = store.setDeep(location, instr.local.asLocal)
              loopStep((), step, stack, store2)

            case Tags.LocalUpdate =>
              val instr = comp2.asInstanceOf[CC.LocalUpdate[Any, Any]]
              //@#@OPTY Store.update
              val local = store.getDeep(location)
              val (value, local2) = instr.fun(local)
              val store2 = store.setDeep(location, local2.asLocal)
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
            case halt: Halt.Loop => halt
            case (tag2, payload2, step2, stack2, store2) => innerLoop(tag2, payload2, step2, stack2, store2)
    else
      if currentTickHigh > 0 then
        currentTickHigh -= 1
        currentTickLow = currentEnv.tickLow
        if currentFiber.cancellationCheck() then
          loopCancel(stack, store)
        else
          innerLoop(tag, payload, step, stack, store)
      else
        currentFiber.suspend(tag, payload, step, stack, store)
        Halt.Yield(currentFiber)


  private def loopMore(tag: Byte, payload: Any, step: Step, stack: Stack, store: Store): LoopMore =
    inline def loopStep(payload: Any, step: Step, stack: Stack, store: Store): LoopMore =
      (step.tag, payload, step, stack, store)

    inline def loopComp(comp: Computation[?, ?], step: Step, stack: Stack, store: Store): LoopMore =
      (comp.tag, comp, step, stack, store)

    inline def loopCancel(stack: Stack, store: Store): LoopMore =
      (Tags.Step_Unwind, CancelPayload, Step.Cancel, stack, store)

    inline def innerLoop(tag: Byte, payload: Any, step: Step, stack: Stack, store: Store): LoopMore =
      (tag, payload, step, stack, store)

    (tag: @switch) match
      case Tags.LocalGet =>
        val instr = payload.asInstanceOf[CC.LocalGet]
        val location = stack.locatePrompt(instr.prompt)
        val local = store.getDeep(location)
        loopStep(local, step, stack, store)

      case Tags.LocalPut =>
        val instr = payload.asInstanceOf[CC.LocalPut[Any]]
        val location = stack.locatePrompt(instr.prompt)
        val store2 = store.setDeep(location, instr.local.asLocal)
        loopStep((), step, stack, store2)

      case Tags.LocalUpdate =>
        val instr = payload.asInstanceOf[CC.LocalUpdate[Any, Any]]
        val location = stack.locatePrompt(instr.prompt)
        //@#@OPTY Store.update
        val local = store.getDeep(location)
        val (value, local2) = instr.fun(local)
        val store2 = store.setDeep(location, local2.asLocal)
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
          if currentFiber.tryStartRace(fiberLeft, fiberRight) then
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
          if currentFiber.tryStartRace(fiberLeft, fiberRight) then
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
        if currentFiber.tryStartRaceOfOne(fiberLeft) then
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
            frame.kind.unwrap match
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
                  case Warp.ExitMode.Cancel => warp.tryGetCancelledBy(currentFiber)
                  case Warp.ExitMode.Shutdown => warp.tryGetAwaitedBy(currentFiber)
                  case _ => impossible //// this is a scoped warp, so it must have ExitMode
                tried match
                  case Bits.WaiterSubscribed => Halt.ThreadDisowned
                  case Bits.WaiterAlreadyCancelled => impossible //// Latch is set
                  case Bits.WaiteeAlreadyCompleted =>
                    currentFiber.clearSuspension()
                    refreshEnv(stack2, store2)
                    loopStep(payload, fallthrough, stack2, store2)

              case FrameKind.EXEC =>
                currentFiber.suspend(fallthrough.tag, payload, fallthrough, stack2, store2)
                currentFiber.resume()
                Halt.ThreadDisowned
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
          ovar.tryGetAwaitedBy(currentFiber) match
            case Bits.WaiterSubscribed => Halt.ThreadDisowned
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
          val tag2: Byte = if instr.isVoid then Tags.NotifyFiberVoid else Tags.NotifyFiber
          currentFiber.suspend(tag2, waitee, step, stack, store)
          val tried =
            if instr.isCancel
            then waitee.tryGetCancelledBy(currentFiber)
            else waitee.tryGetAwaitedBy(currentFiber)
          tried match
            case Bits.WaiterSubscribed => Halt.ThreadDisowned
            case Bits.WaiterAlreadyCancelled =>
              currentFiber.clearSuspension()
              loopCancel(stack, store)
            case Bits.WaiteeAlreadyCompleted =>
              currentFiber.clearSuspension()
              val payload2 = if instr.isVoid then () else waitee.getOrMakeZipper
              loopStep(payload2, step, stack, store)
        else
          if instr.isCancel then
            currentFiber.cancelBySelf()
            loopCancel(stack, store)
          else
            val zombie = new Blocker.Zombie(currentFiber)
            currentFiber.suspend(step.tag, zombie, step, stack, store)
            if currentFiber.tryGetBlocked() then
              Halt.ThreadDisowned
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
        val oldWarp = currentEnv.currentWarp
        val parent: WarpImpl | FiberImpl = if oldWarp != null then oldWarp.nn else currentFiber
        val newWarp = new WarpImpl(parent, instr.name, instr.exitMode)
        if oldWarp != null then
          oldWarp.nn.tryAddWarp(newWarp)
        val newEnv = currentEnv.copy(currentWarp = newWarp)
        val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, newEnv.asLocal, FrameKind.warp)
        this.currentEnv = newEnv
        loopComp(instr.body, SC.Pop, stack2, store2)

      case Tags.AwaitWarp =>
        val instr = payload.asInstanceOf[CC.AwaitWarp]
        val warp = instr.warp.asImpl
        currentFiber.suspend(step.tag, (), step, stack, store)
        val tried =
          if instr.isCancel
          then warp.tryGetCancelledBy(currentFiber)
          else warp.tryGetAwaitedBy(currentFiber)
        tried match
          case Bits.WaiterSubscribed => Halt.ThreadDisowned
          case Bits.WaiterAlreadyCancelled =>
            currentFiber.clearSuspension()
            loopCancel(stack, store)
          case Bits.WaiteeAlreadyCompleted =>
            currentFiber.clearSuspension()
            loopStep((), step, stack, store)

      case Tags.Blocking =>
        val instr = payload.asInstanceOf[CC.Blocking[Any, Any]]
        val blocker = new Blocker.Interruptible(currentFiber, instr.thunk)
        currentFiber.suspend(Tags.NotifyBlocker, blocker, step, stack, store)
        if currentFiber.tryGetBlocked() then
          blocker.block()
          Halt.ThreadDisowned
        else
          currentFiber.clearSuspension()
          loopCancel(stack, store)

      case Tags.Sleep =>
        val instr = payload.asInstanceOf[CC.Sleep]
        val blocker = new Blocker.Sleeper(currentFiber)
        currentFiber.suspend(Tags.NotifyBlocker, blocker, step, stack, store)
        if currentFiber.tryGetBlocked() then
          blocker.sleep(instr.length, instr.unit)
          Halt.ThreadDisowned
        else
          currentFiber.clearSuspension()
          loopCancel(stack, store)

      case Tags.NotifyBlocker =>
        val blocker = payload.asInstanceOf[Blocker]
        val e = blocker.throwable
        if e == null then
          val payload2 = blocker.result
          loopStep(payload2, step, stack, store)
        else
          val step2 = Step.Throw
          val payload2 = Cause(e)
          loopStep(payload2, step2, stack, store)

      case Tags.NotifyBlockerAttempt =>
        val blocker = payload.asInstanceOf[Blocker]
        val payload2 = blocker.toEither
        loopStep(payload2, step, stack, store)

      case Tags.ExecOn =>
        val instr = payload.asInstanceOf[CC.ExecOn[Any, Any]]
        if currentEnv.executor == instr.exec then
          loopComp(instr.body, step, stack, store)
        else
          val env2 = currentEnv.copy(executor = instr.exec)
          val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2.asLocal, FrameKind.exec)
          currentFiber.suspend(instr.body.tag, instr.body, SC.Pop, stack2, store2)
          currentFiber.resume()
          Halt.ThreadDisowned

      case Tags.Yield =>
        currentFiber.suspend(step.tag, (), step, stack, store)
        Halt.Yield(currentFiber)


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


  private def endOfLoop(completion: Int, payload: Any, stack: Stack | Null): Halt.Loop =
    currentFiber.doFinalize(completion, payload, stack) match
      case null => Halt.retire(currentFiber.isReentry)
      case fiber2 => become(fiber2.nn); Halt.Become


  def becomeWithSameEnv(fiber: FiberImpl): Unit =
    currentFiber = fiber


  def become(fiber: FiberImpl): Unit =
    currentFiber = fiber
    refreshEnv(fiber.suspendedStack.nn, fiber.suspendedStore.nn) 


  def refreshEnv(stack: Stack, store: Store): Unit =
    currentEnv = OpPush.findTopmostEnv(stack, store)


private object MainLoop:
  type LoopMore = Halt.Loop | (Byte, Any, Step, Stack, Store)