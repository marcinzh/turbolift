package turbolift.internals.engine
import scala.annotation.{tailrec, switch}
import turbolift.{Computation, Signature}
import turbolift.io.{Fiber, Zipper, Warp, OnceVar, Snap, Outcome, Cause, Exceptions}
import turbolift.internals.primitives.{Tags, ComputationCases => CC}
import turbolift.internals.executor.Resumer
import turbolift.internals.engine.{StepCases => SC}
import Cause.{Cancelled => CancelPayload}


private[turbolift] final class FiberImpl private (
  private val constantBits: Byte,
  private var theParent: WarpImpl | FiberImpl,
  private var theName: String,
) extends ChildLink with Fiber.Unsealed:
  private var suspendedTag: Byte = 0
  private var suspendedPayload: Any = null
  private var suspendedStep: Step | Null = null
  private var suspendedStack: Stack | Null = null
  private var suspendedStore: Store | Null = null


  def run(): Halt =
    assert(isSuspended)
    theOwnership = Bits.Ownership_Self
    outerLoop(0, 0, new Cache)


  @tailrec private def outerLoop(tickHigh: Short, lastTickLow: Short, cache: Cache): Halt =
    val nextTickLow: Short =
      if lastTickLow == 0 then
        if cancellationCheck() then
          suspendAsCancelled()
        suspendedStore.nn.getEnv.tickLow
      else
        lastTickLow

    val result =
      val currentTag     = suspendedTag
      val currentPayload = suspendedPayload.nn
      val currentStep    = suspendedStep.nn
      val currentStack   = suspendedStack.nn
      val currentStore   = suspendedStore.nn
      clearSuspension()
      try
        innerLoop(
          tick     = nextTickLow,
          tag      = currentTag,
          payload  = currentPayload,
          step     = currentStep,
          stack    = currentStack,
          store    = currentStore,
          cache    = cache,
        )
      catch e =>
        e.printStackTrace()
        val e2 = if e.isInstanceOf[Exceptions.Panic] then e else new Exceptions.Unhandled(e)
        val c = Cause(e2)
        endOfFiber(0, Bits.Completion_Failure, c, null)

    result match
      case Halt.Reset =>
        if tickHigh < suspendedStore.nn.getEnv.tickHigh then
          outerLoop((tickHigh + 1).toShort, 0, cache)
        else
          Halt.Yield(this)
      case Halt.Become(that, tickLow2) => that.outerLoop(tickHigh, tickLow2, cache)
      case halt: Halt => halt


  //-------------------------------------------------------------------
  // Inner Loop
  //-------------------------------------------------------------------


  @tailrec private def innerLoop(
    tick: Short,
    tag: Byte,
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
    cache: Cache,
  ): Halt.Loop =
    if tick > 0 then
      val tick2 = (tick - 1).toShort

      inline def loopStep(payload: Any, step: Step, stack: Stack, store: Store, cache: Cache): Halt.Loop =
        innerLoop(tick2, step.tag, payload, step, stack, store, cache)

      inline def loopComp(
        comp: Computation[?, ?], step: Step, stack: Stack, store: Store, cache: Cache): Halt.Loop =
        innerLoop(tick2, comp.tag, comp, step, stack, store, cache)

      inline def loopCancel(stack: Stack, store: Store, cache: Cache): Halt.Loop =
        innerLoop(tick2, Tags.Step_Unwind, CancelPayload, Step.Cancel, stack, store, cache)

      (tag: @switch) match
        case Tags.MapFlat =>
          val theMap = payload.asInstanceOf[CC.Map[Any, Any]]
          val comp2 = theMap.comp
          val step2 = SC.More(Tags.Step_MoreFlat, theMap.fun, step)
          loopComp(comp2, step2, stack, store, cache)

        case Tags.MapPure =>
          val theMap = payload.asInstanceOf[CC.Map[Any, Any]]
          val comp2 = theMap.comp
          val step2 = SC.More(Tags.Step_MorePure, theMap.fun, step)
          loopComp(comp2, step2, stack, store, cache)

        case Tags.Step_MoreFlat =>
          val theMore = step.asInstanceOf[SC.More]
          val step2 = theMore.next
          val comp2 = theMore.fun(payload).asInstanceOf[AnyComp]
          loopComp(comp2, theMore.next, stack, store, cache)

        case Tags.Step_MorePure =>
          val theMore = step.asInstanceOf[SC.More]
          val step2 = theMore.next
          val payload2 = theMore.fun(payload)
          loopStep(payload2, step2, stack, store, cache)

        case Tags.Perform =>
          val thePerform = payload.asInstanceOf[CC.Perform[Any, Any, Signature]]
          stack.locateSignature(thePerform.sig, cache)
          val comp2 = thePerform.op(cache.interpreter).asInstanceOf[AnyComp]
          loopComp(comp2, step, stack, store, cache)

        case Tags.Pure =>
          val thePure = payload.asInstanceOf[CC.Pure[Any]]
          val payload2 = thePure.value
          loopStep(payload2, step, stack, store, cache)

        case Tags.Impure =>
          val theImpure = payload.asInstanceOf[CC.Impure[Any, Any]]
          val payload2 = theImpure.thunk()
          loopStep(payload2, step, stack, store, cache)

        case Tags.LocalGet =>
          val theLocalGet = payload.asInstanceOf[CC.LocalGet]
          stack.locatePrompt(theLocalGet.interp, cache)
          val local = store.getDeep(cache.location)
          loopStep(local, step, stack, store, cache)

        case Tags.LocalPut =>
          val theLocalPut = payload.asInstanceOf[CC.LocalPut[Any]]
          stack.locatePrompt(theLocalPut.interp, cache)
          val store2 = store.setDeep(cache.location, theLocalPut.local.asLocal)
          loopStep((), step, stack, store2, cache)

        case Tags.LocalUpdate =>
          val theLocalUpdate = payload.asInstanceOf[CC.LocalUpdate[Any, Any]]
          stack.locatePrompt(theLocalUpdate.interp, cache)
          //@#@OPTY Store.update
          val local = store.getDeep(cache.location)
          val (value, local2) = theLocalUpdate.fun(local)
          val store2 = store.setDeep(cache.location, local2.asLocal)
          loopStep(value, step, stack, store2, cache)

        case Tags.Delimit =>
          val theDelimit = payload.asInstanceOf[CC.Delimit[Any, Local, Any]]
          stack.locatePrompt(theDelimit.interp, cache)
          val local =
            if theDelimit.fun == null
            then theDelimit.local
            else theDelimit.fun(store.getDeep(cache.location))
          val (stack2, store2) = OpPush.pushNested(stack, store, step, cache.prompt, cache.location, local, FrameKind.plain)
          loopComp(theDelimit.body, SC.Pop, stack2, store2, cache)

        case Tags.Abort =>
          val theAbort = payload.asInstanceOf[CC.Abort[Any, Any]]
          stack.locatePrompt(theAbort.interp, cache)
          val step2 = cache.prompt.unwind
          val payload2 = theAbort.value
          loopStep(payload2, step2, stack, store, cache)

        case Tags.Resume =>
          val theResume = payload.asInstanceOf[CC.Resume[Any, Any, Local, Any]]
          val cont = theResume.cont.asImpl
          val (stack2, store2) = OpSplit.merge(
            stackHi = cont.stack,
            storeHi = cont.store.setDeepIfNotVoid(cont.location, theResume.local),
            stepMid = step,
            stackLo = stack,
            storeLo = store,
          )
          loopStep(theResume.value, cont.step, stack2, store2, cache)

        case Tags.Capture =>
          val theCapture = payload.asInstanceOf[CC.Capture[Any, Any, Any, Any]]
          stack.locatePrompt(theCapture.interp, cache)
          val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(stack, store, cache.location)
          stackHi.locatePrompt(theCapture.interp, cache)
          val cont = new ContImpl(stackHi, storeHi, step, cache.location)
          val comp2 = (theCapture.fun: @unchecked) match
            case f: Function1[Any, AnyComp] => f(cont)
            case f: Function2[Any, Any, AnyComp] =>
              val local = storeHi.getDeep(cache.location)
              f(cont, local)
          loopComp(comp2, stepMid, stackLo, storeLo, cache)

        case Tags.Step_Bridge =>
          val (stack2, store2, step2) = OpPush.drop(stack, store)
          loopStep(payload, step2, stack2, store2, cache)

        case Tags.ZipPar =>
          val theZipPar = payload.asInstanceOf[CC.ZipPar[Any, Any, Any, Any]]
          //@#@TODO Too conservative? Should check for `features.isParallel` at `mark`, instead of at stack top
          if stack.accumFeatures.isParallel && store.getEnv.isParallelismRequested then
            val fiberLeft = new FiberImpl(Bits.ZipPar_Left, this, "")
            val fiberRight = new FiberImpl(Bits.ZipPar_Right, this, "")
            if tryStartRace(fiberLeft, fiberRight) then
              val (storeTmp, storeLeft) = OpCascaded.fork(stack, store)
              val (storeDown, storeRight) = OpCascaded.fork(stack, storeTmp)
              suspendForRace(theZipPar.fun, step, stack, storeDown)
              val stack2 = stack.makeFork
              fiberRight.suspend(theZipPar.rhs.tag, theZipPar.rhs, SC.Pop, stack2, storeRight)
              fiberRight.resume()
              fiberLeft.innerLoop(tick2, theZipPar.lhs.tag, theZipPar.lhs, SC.Pop, stack2, storeLeft, cache)
            else
              //// Must have been cancelled meanwhile
              loopCancel(stack, store, cache)
          else
            //// Fallback to sequential
            val comp2 = CC.ZipSeq(theZipPar.lhs, () => theZipPar.rhs, theZipPar.fun)
            loopComp(comp2, step, stack, store, cache)

        case Tags.ZipSeq =>
          val theZipSeq = payload.asInstanceOf[CC.ZipSeq[Any, Any, Any, Any]]
          val step2 = SC.ZipSeqLeft(theZipSeq.rhsFun, theZipSeq.fun, step)
          val comp2 = theZipSeq.lhs
          loopComp(comp2, step2, stack, store, cache)

        case Tags.OrPar =>
          val theOrPar = payload.asInstanceOf[CC.OrPar[Any, Any]]
          if stack.accumFeatures.isParallel && store.getEnv.isParallelismRequested then
            val fiberLeft = new FiberImpl(Bits.OrPar_Left, this, "")
            val fiberRight = new FiberImpl(Bits.OrPar_Right, this, "")
            if tryStartRace(fiberLeft, fiberRight) then
              val (storeTmp, storeLeft) = OpCascaded.fork(stack, store)
              val (storeDown, storeRight) = OpCascaded.fork(stack, storeTmp)
              suspendForRace(null, step, stack, storeDown)
              val stack2 = stack.makeFork
              fiberRight.suspend(theOrPar.rhs.tag, theOrPar.rhs, SC.Pop, stack2, storeRight)
              fiberRight.resume()
              fiberLeft.innerLoop(tick2, theOrPar.lhs.tag, theOrPar.lhs, SC.Pop, stack2, storeLeft, cache)
            else
              //// Must have been cancelled meanwhile
              loopCancel(stack, store, cache)
          else
            //// Fallback to sequential
            val comp2 = CC.OrSeq(theOrPar.lhs, () => theOrPar.rhs)
            loopComp(comp2, step, stack, store, cache)

        case Tags.OrSeq =>
          val theOrSeq = payload.asInstanceOf[CC.OrSeq[Any, Any]]
          val fiberLeft = new FiberImpl(Bits.OrSeq, this, "")
          if tryStartRaceOfOne(fiberLeft) then
            val (storeDown, storeLeft) = OpCascaded.fork(stack, store)
            suspendForRace(theOrSeq.rhsFun, step, stack, storeDown)
            val stack2 = stack.makeFork
            fiberLeft.innerLoop(tick2, theOrSeq.lhs.tag, theOrSeq.lhs, SC.Pop, stack2, storeLeft, cache)
          else
            //// Must have been cancelled meanwhile
            loopCancel(stack, store, cache)

        case Tags.Step_ZipSeqLeft =>
          val theZipSeqLeft = step.asInstanceOf[SC.ZipSeqLeft]
          val comp2 = theZipSeqLeft.todoRight()
          val step2 = SC.ZipSeqRight(payload, theZipSeqLeft.fun, theZipSeqLeft.next)
          loopComp(comp2, step2, stack, store, cache)

        case Tags.Step_ZipSeqRight =>
          val theZipSeqRight = step.asInstanceOf[SC.ZipSeqRight]
          val step2 = theZipSeqRight.next
          val payload2 = theZipSeqRight.fun(theZipSeqRight.doneLeft, payload)
          loopStep(payload2, step2, stack, store, cache)

        case Tags.Handle =>
          val theHandle = payload.asInstanceOf[CC.Handle[Any, Any, [_] =>> Any, [_] =>> Any, Any, Any]]
          val interpreter = theHandle.handler.interpreter.untyped
          val prompt = new Prompt(interpreter)
          for sig <- prompt.interpreter.signatures do
            if stack.containsSignature(sig) then
              panic(s"Unsupported feature: shadowing effect ${sig}.")
          val comp2 = interpreter.onInitial
          val step2 = new SC.Push(theHandle.body, prompt, step)
          loopComp(comp2, step2, stack, store, cache)

        case Tags.Step_Push =>
          val thePush = step.asInstanceOf[SC.Push]
          val step2 = thePush.next
          val (stack2, store2) = OpPush.pushBase(stack, store, step2, thePush.prompt, payload.asLocal)
          val comp2 = thePush.body
          loopComp(comp2, SC.Pop, stack2, store2, cache)

        case Tags.Step_Unwind =>
          val theUnwind = step.asInstanceOf[SC.Unwind]
          if stack.canPop then
            val (stack2, store2, step2, prompt, frame, local) = OpPush.pop(stack, store)
            if prompt.isIo then
              frame.kind.unwrap match
                case FrameKind.PLAIN =>
                  val step3 = if theUnwind.kind.isPop then step2 else step
                  loopStep(payload, step3, stack2, store2, cache)

                case FrameKind.GUARD =>
                  val payload2 = theUnwind.kind match
                    case Step.UnwindKind.Pop    => Snap.Success(payload)
                    case Step.UnwindKind.Abort  => Snap.Aborted(payload, theUnwind.prompt.nn)
                    case Step.UnwindKind.Cancel => Snap.Cancelled
                    case Step.UnwindKind.Throw  => Snap.Failure(payload.asInstanceOf[Cause])
                  loopStep(payload2, step2, stack2, store2, cache)

                case FrameKind.WARP =>
                  val step3 = if theUnwind.kind.isPop then step2 else step
                  suspend(step3.tag, payload, step3, stack2, store2)
                  store.getEnv.currentWarp.tryGetCancelledBy(this) match
                    case Bits.WaiterSubscribed => Halt.ThreadDisowned
                    case Bits.WaiterAlreadyCancelled => impossible //// Latch is set
                    case Bits.WaiteeAlreadyCompleted =>
                      clearSuspension()
                      loopStep(payload, step3, stack2, store2, cache)
            else
              if theUnwind.kind.isPop then
                val comp2 = prompt.interpreter.onReturn(payload, local)
                loopComp(comp2, step2, stack2, store2, cache)
              else
                val step3 = if prompt == theUnwind.prompt then step2 else step
                loopStep(payload, step3, stack2, store2, cache)
          else
            val completion = theUnwind.kind match
              case Step.UnwindKind.Pop    => Bits.Completion_Success
              case Step.UnwindKind.Abort  => impossible
              case Step.UnwindKind.Cancel => Bits.Completion_Cancelled
              case Step.UnwindKind.Throw  => Bits.Completion_Failure
            endOfFiber(tick2, completion, payload, stack)

        case _ => (tag: @switch) match
          case Tags.DoIO =>
            val theDoIO = payload.asInstanceOf[CC.DoIO[Any, Any]]
            var result: Any = null
            var throwable: Throwable | Null = null
            try
              result = theDoIO.thunk()
            catch
              case e => throwable = e
            if throwable == null then
              val payload2 = if theDoIO.isAttempt then Right(result) else result
              loopStep(payload2, step, stack, store, cache)
            else
              if theDoIO.isAttempt then
                val payload2 = Left(throwable)
                loopStep(payload2, step, stack, store, cache)
              else
                val step2 = Step.Throw
                val payload2 = Cause(throwable.nn)
                loopStep(payload2, step2, stack, store, cache)

          case Tags.DoSnap =>
            val theDoSnap = payload.asInstanceOf[CC.DoSnap[Any, Any]]
            val location = stack.locateIO
            val (stack2, store2) = OpPush.pushNested(stack, store, step, Prompt.io, location, store.getEnvAsLocal, FrameKind.guard)
            loopComp(theDoSnap.body, SC.Pop, stack2, store2, cache)

          case Tags.Unsnap =>
            val theUnsnap = payload.asInstanceOf[CC.Unsnap[Any, Any]]
            //@#@TODO forbid uncancelling, it wouldnt work correctly anyway
            theUnsnap.snap match
              case Snap.Success(payload2) =>
                loopStep(payload2, step, stack, store, cache)
              case Snap.Failure(payload2) =>
                val step2 = Step.Throw
                loopStep(payload2, step2, stack, store, cache)
              case theAborted: Snap.Aborted =>
                val payload2 = theAborted.value
                val step2 = theAborted.prompt.unwind
                loopStep(payload2, step2, stack, store, cache)
              case Snap.Cancelled =>
                cancelBySelf()
                loopCancel(stack, store, cache)

          case Tags.EnvAsk =>
            val theEnvAsk = payload.asInstanceOf[CC.EnvAsk[Any]]
            val value = theEnvAsk.fun(store.getEnv)
            loopStep(value, step, stack, store, cache)

          case Tags.EnvMod =>
            val theEnvMod = payload.asInstanceOf[CC.EnvMod[Any, Any]]
            val env1 = store.getEnv
            val env2 = theEnvMod.fun(env1)
            if env1 == env2 then
              loopComp(theEnvMod.body, step, stack, store, cache)
            else
              val location = stack.locateIO
              val (stack2, store2) = OpPush.pushNested(stack, store, step, Prompt.io, location, env2.asLocal, FrameKind.plain)
              loopComp(theEnvMod.body, SC.Pop, stack2, store2, cache)

          case Tags.AwaitOnceVar =>
            val theOnceVarGet = payload.asInstanceOf[CC.AwaitOnceVar[Any]]
            val ovar = theOnceVarGet.ovar.asImpl
            val value = ovar.theContent
            if OnceVarImpl.Empty != value then
              loopStep(value, step, stack, store, cache)
            else
              suspend(Tags.NotifyOnceVar, ovar, step, stack, store)
              ovar.tryGetAwaitedBy(this) match
                case Bits.WaiterSubscribed => Halt.ThreadDisowned
                case Bits.WaiterAlreadyCancelled =>
                  clearSuspension()
                  loopCancel(stack, store, cache)
                case Bits.WaiteeAlreadyCompleted =>
                  clearSuspension()
                  val value = ovar.theContent
                  loopStep(value, step, stack, store, cache)

          case Tags.NotifyOnceVar =>
            val ovar = payload.asInstanceOf[OnceVarImpl]
            val value = ovar.theContent
            loopStep(value, step, stack, store, cache)

          case Tags.ForkFiber =>
            val theForkFiber = payload.asInstanceOf[CC.ForkFiber[Any, Any]]
            val warp = if theForkFiber.warp != null then theForkFiber.warp.nn.asImpl else store.getEnv.currentWarp
            val (storeDown, storeFork) = OpCascaded.fork(stack, store)
            val stackFork = stack.makeFork
            val child = new FiberImpl(Bits.Tree_Explicit.toByte, warp, theForkFiber.name)
            child.suspend(theForkFiber.comp.tag, theForkFiber.comp, SC.Pop, stackFork, storeFork)
            if warp.tryAddFiber(child) then
              child.resume()
              loopStep(child, step, stack, storeDown, cache)
            else
              child.suspendAsCancelled()
              loopStep(child, step, stack, store, cache)

          case Tags.AwaitFiber =>
            val theAwaitFiber = payload.asInstanceOf[CC.AwaitFiber[Any, Any]]
            val waitee = theAwaitFiber.fiber.asImpl
            if this != waitee then
              val tag2: Byte = if theAwaitFiber.isVoid then Tags.NotifyFiberVoid else Tags.NotifyFiber
              suspend(tag2, waitee, step, stack, store)
              val tried =
                if theAwaitFiber.isCancel
                then waitee.tryGetCancelledBy(this)
                else waitee.tryGetAwaitedBy(this)
              tried match
                case Bits.WaiterSubscribed => Halt.ThreadDisowned
                case Bits.WaiterAlreadyCancelled =>
                  clearSuspension()
                  loopCancel(stack, store, cache)
                case Bits.WaiteeAlreadyCompleted =>
                  clearSuspension()
                  val payload2 = if theAwaitFiber.isVoid then () else waitee.getOrMakeZipper
                  loopStep(payload2, step, stack, store, cache)
            else
              if theAwaitFiber.isCancel then
                cancelBySelf()
                loopCancel(stack, store, cache)
              else
                val zombie = new Blocker.Zombie(this)
                suspend(step.tag, zombie, step, stack, store)
                if tryGetBlocked() then
                  Halt.ThreadDisowned
                else
                  clearSuspension()
                  loopCancel(stack, store, cache)

          case Tags.NotifyFiber =>
            val waitee = payload.asInstanceOf[FiberImpl]
            val zipper = waitee.getOrMakeZipper
            loopStep(zipper, step, stack, store, cache)

          case Tags.NotifyFiberVoid =>
            loopStep((), step, stack, store, cache)

          case Tags.CurrentFiber =>
            loopStep(this, step, stack, store, cache)

          case Tags.SpawnWarp =>
            val theSpawnWarp = payload.asInstanceOf[CC.SpawnWarp[Any, Any]]
            val oldEnv = store.getEnv
            val oldWarp = oldEnv.currentWarp
            val newWarp = new WarpImpl(oldWarp, theSpawnWarp.name)
            if oldWarp.tryAddWarp(newWarp) then
              val location = stack.locateIO
              val newEnv = oldEnv.copy(currentWarp = newWarp)
              val (stack2, store2) = OpPush.pushNested(stack, store, step, Prompt.io, location, newEnv.asLocal, FrameKind.warp)
              loopComp(theSpawnWarp.body, SC.Pop, stack2, store2, cache)
            else
              impossible

          case Tags.AwaitWarp =>
            val theAwaitWarp = payload.asInstanceOf[CC.AwaitWarp]
            val warp = theAwaitWarp.warp.asImpl
            suspend(step.tag, (), step, stack, store)
            val tried =
              if theAwaitWarp.isCancel
              then warp.tryGetCancelledBy(this)
              else warp.tryGetAwaitedBy(this)
            tried match
              case Bits.WaiterSubscribed => Halt.ThreadDisowned
              case Bits.WaiterAlreadyCancelled =>
                clearSuspension()
                loopCancel(stack, store, cache)
              case Bits.WaiteeAlreadyCompleted =>
                clearSuspension()
                loopStep((), step, stack, store, cache)

          case Tags.Blocking =>
            val theBlocking = payload.asInstanceOf[CC.Blocking[Any, Any]]
            val blocker = new Blocker.Interruptible(this, theBlocking.thunk)
            suspend(Tags.NotifyBlocker, blocker, step, stack, store)
            if tryGetBlocked() then
              blocker.block()
              Halt.ThreadDisowned
            else
              clearSuspension()
              loopCancel(stack, store, cache)

          case Tags.Sleep =>
            val theSleep = payload.asInstanceOf[CC.Sleep]
            val blocker = new Blocker.Sleeper(this)
            suspend(Tags.NotifyBlocker, blocker, step, stack, store)
            if tryGetBlocked() then
              blocker.sleep(theSleep.length, theSleep.unit)
              Halt.ThreadDisowned
            else
              clearSuspension()
              loopCancel(stack, store, cache)

          case Tags.NotifyBlocker =>
            val blocker = payload.asInstanceOf[Blocker]
            val e = blocker.throwable
            if e == null then
              val payload2 = blocker.result
              loopStep(payload2, step, stack, store, cache)
            else
              val step2 = Step.Throw
              val payload2 = Cause(e)
              loopStep(payload2, step2, stack, store, cache)

          case Tags.NotifyBlockerAttempt =>
            val blocker = payload.asInstanceOf[Blocker]
            val payload2 = blocker.toEither
            loopStep(payload2, step, stack, store, cache)

          case Tags.Yield =>
            suspend(step.tag, (), step, stack, store)
            Halt.Yield(this)

    else
      suspend(tag, payload, step, stack, store)
      Halt.Reset


  //-------------------------------------------------------------------
  // Finalization
  //-------------------------------------------------------------------


  private def endOfFiber(tick: Short, completion: Int, initialPayload: Any, stack: Stack | Null): Halt.Loop =
    val payload =
      if isExplicit then
        ZipperImpl.make(stack, initialPayload, completion)
      else
        initialPayload

    val cancelPayload =
      if isExplicit then
        ZipperCases.Cancelled
      else
        CancelPayload

    synchronized {
      if isCancellationUnlatched then
        //// If cancellation was signalled before reaching completion, it overrides the completion.
        varyingBits = (varyingBits | Bits.Completion_Cancelled | Bits.Cancellation_Latch).toByte
        suspendedPayload = cancelPayload
      else
        varyingBits = (varyingBits | completion).toByte
        suspendedPayload = payload
    }

    //// As a RACER:
    val isLastRacer = if isRacer then endRace() else false

    //// As a WAITEE:
    notifyAllWaiters()

    //// As a RACER or CHILD:
    theParent match
      case arbiter: FiberImpl =>
        if isLastRacer then
          Halt.Become(arbiter, tick)
        else
          retire

      case warp: WarpImpl =>
        warp.removeFiber(this)
        if isRoot then
          warp.unsafeCancelAndForget()
        retire


  private def retire: Halt = Halt.retire(isReentry)


  def makeOutcome[A]: Outcome[A] = makeOutcome(false)
  

  private def makeOutcome[A](void: Boolean): Outcome[A] =
    getCompletion match
      case Bits.Completion_Success   => Outcome.Success((if void then null else suspendedPayload).asInstanceOf[A])
      case Bits.Completion_Failure   => Outcome.Failure(suspendedPayload.asInstanceOf[Cause])
      case Bits.Completion_Cancelled => Outcome.Cancelled


  private def getOrMakeZipper: ZipperImpl =
    if isExplicit then
      suspendedPayload.asInstanceOf[ZipperImpl]
    else
      ZipperImpl.make(Stack.initial, suspendedPayload, getCompletion)


  private def getOrMakeZipper(payload: Any, completion: Int): ZipperImpl =
    if isExplicit then
      suspendedPayload.asInstanceOf[ZipperImpl]
    else
      ZipperImpl.make(Stack.initial, payload, completion)


  //-------------------------------------------------------------------
  // Awaiting
  //-------------------------------------------------------------------


  private def tryGetBlocked(): Boolean =
    synchronized {
      if isCancellationUnlatched then
        setCancellationLatch()
        false
      else
        theOwnership = Bits.Ownership_Blocker
        true
    }


  private def tryGetAwaitedBy(waiter: FiberImpl): Int =
    waiter.synchronized {
      if isCancellationUnlatched then
        setCancellationLatch()
        Bits.WaiterAlreadyCancelled
      else
        synchronized {
          if isPending then
            subscribeWaiterUnsync(waiter)
            Bits.WaiterSubscribed
          else
            Bits.WaiteeAlreadyCompleted
         }
    }


  //-------------------------------------------------------------------
  // Cancelling
  //-------------------------------------------------------------------


  private[engine] def isCancellationUnlatched: Boolean = Bits.isCancellationUnlatched(varyingBits)


  private[engine] def setCancellationLatch(): Unit =
    varyingBits = (varyingBits | Bits.Cancellation_Latch).toByte


  private def cancellationCheck(): Boolean =
    synchronized {
      if isCancellationUnlatched then
        setCancellationLatch()
        true
      else
        false
    }


  private def cancelBySelf(): Unit =
    synchronized {
      varyingBits = (varyingBits | Bits.Cancellation_Signal | Bits.Cancellation_Latch).toByte
    }


  private def cancelByWinner(): Unit =
    deepCancelLoop(this)


  private def tryGetCancelledBy(canceller: FiberImpl): Int =
    var savedLeftRacer: WaiterLink | Null = null
    var savedRightRacer: WaiterLink | Null = null
    var savedVaryingBits: Byte = 0
    var savedOwnership: Byte = 0
    var savedPayload: Any = null
    var willDescend = false

    val result =
      canceller.synchronized {
        if canceller.isCancellationUnlatched then
          canceller.setCancellationLatch()
          Bits.WaiterAlreadyCancelled
        else
          synchronized {
            if isPending then
              if !isCancelled then
                varyingBits = (varyingBits | Bits.Cancellation_Signal).toByte
                willDescend = true
                savedLeftRacer = prevWaiter
                savedRightRacer = nextWaiter
                savedOwnership = theOwnership
                savedVaryingBits = varyingBits
                savedPayload = suspendedPayload
              subscribeWaiterUnsync(canceller)
              Bits.WaiterSubscribed
            else
              Bits.WaiteeAlreadyCompleted
           }
      }

    if willDescend then
      val racer = doDescend(savedLeftRacer, savedRightRacer, savedVaryingBits, savedOwnership, savedPayload)
      if racer != null then
        racer.deepCancelLoop(this)
    result


  //// Same as `tryGetCancelledBy(canceller)`, except:
  //// - doesn't synchronize on the `canceller`
  //// - doesn't subscribe the `canceller`
  //// - doesn't initiate `deepCancelLoop`
  //// - returns first pending racer, instead of Int code
  private[engine] override def deepCancelDown(): ChildLink | Null =
    var savedLeftRacer: WaiterLink | Null = null
    var savedRightRacer: WaiterLink | Null = null
    var savedVaryingBits: Byte = 0
    var savedOwnership: Byte = 0
    var savedPayload: Any = null

    val willDescend =
      synchronized {
        if isPendingAndNotCancelled then
          varyingBits = (varyingBits | Bits.Cancellation_Signal).toByte
          savedLeftRacer = prevWaiter
          savedRightRacer = nextWaiter
          savedOwnership = theOwnership
          savedVaryingBits = varyingBits
          savedPayload = suspendedPayload
          true
        else
          false
      }

    if willDescend then
      doDescend(savedLeftRacer, savedRightRacer, savedVaryingBits, savedOwnership, savedPayload)
    else
      null


  private[engine] override def deepCancelRight(): ChildLink | Null =
    if whichRacerAmI == Bits.Racer_Left then
      getArbiter.tryGetRightRacer
    else
      //// Equals null for the right RACER, bcoz racer's parent is a fiber, not a warp.
      nextChild


  private def tryGetRightRacer: FiberImpl | Null =
    synchronized {
      if isPending && (getArbiterBits == Bits.Arbiter_Right) then
        getRightRacer
      else
        null
    }


  private[engine] override def deepCancelUp(): ChildLink =
    theParent match
      case fiber: FiberImpl => fiber
      case warp: WarpImpl => warp


  private def doDescend(
    savedLeftRacer: WaiterLink | Null,
    savedRightRacer: WaiterLink | Null,
    savedVaryingBits: Byte,
    savedOwnership: Byte,
    savedPayload: Any,
  ): FiberImpl | Null =
    savedOwnership match
      case Bits.Ownership_Self =>
        Bits.getArbiter(savedVaryingBits) match
          case Bits.Arbiter_None => null
          case Bits.Arbiter_Right => savedRightRacer.asInstanceOf[FiberImpl]
          case _ => savedLeftRacer.asInstanceOf[FiberImpl]

      case Bits.Ownership_Waitee =>
        val waitee = savedPayload.asInstanceOf[Waitee]
        waitee.unsubscribeWaiter(this)
        null

      case Bits.Ownership_Blocker =>
        val blocker = savedPayload.asInstanceOf[Blocker]
        blocker.unblock()
        null


  //-------------------------------------------------------------------
  // Race
  //-------------------------------------------------------------------

  
  //// Called by the ARBITER on itself
  private def tryStartRace(leftRacer: FiberImpl, rightRacer: FiberImpl): Boolean =
    tryStartRaceExt(leftRacer, rightRacer, Bits.Racer_Both)

  private def tryStartRaceOfOne(leftRacer: FiberImpl): Boolean =
    tryStartRaceExt(leftRacer, null, Bits.Racer_Left)

  private def tryStartRaceExt(leftRacer: FiberImpl, rightRacer: FiberImpl | Null, awaitingBits: Int): Boolean =
    synchronized {
      if isCancellationUnlatched then
        setCancellationLatch()
        false
      else
        varyingBits = (varyingBits | awaitingBits).toByte
        setRacers(leftRacer, rightRacer)
        true
    }


  //// Called by the RACER on its ARBITER
  private def tryWinRace(racerBit: Int): FiberImpl | Null =
    //// If win, return the loser
    synchronized {
      val newBits = varyingBits & ~racerBit
      varyingBits = newBits.toByte
      if (newBits & Bits.Racer_Mask) != 0 then
        //// Win
        if racerBit == Bits.Racer_Left then
          getRightRacer
        else
          getLeftRacer
      else
        //// Loss, or there was no competitor
        null
    }


  //// Called by the RACER on itself
  private def endRace(): Boolean =
    val arbiter = getArbiter
    val racerBit = whichRacerAmI
    val loser = arbiter.tryWinRace(racerBit)
    constantBits & Bits.Tree_Mask match
      case Bits.Tree_ZipPar =>
        if loser != null then
          if getCompletion != Bits.Completion_Success then
            loser.cancelByWinner()
          false
        else
          arbiter.endRaceForZipPar()
          true

      case Bits.Tree_OrPar =>
        if loser != null then
          if getCompletion != Bits.Completion_Cancelled then
            loser.cancelByWinner()
          false
        else
          arbiter.endRaceForOrPar()
          true

      case Bits.Tree_OrSeq =>
        arbiter.endRaceForOrSeq()
        true


  //// Called by the ARBITER on itself
  private def endRaceForZipPar(): Unit =
    val racerLeft = getLeftRacer
    val racerRight = getRightRacer
    val completionLeft = racerLeft.getCompletion
    val completionRight = racerRight.getCompletion
    val payloadLeft = racerLeft.suspendedPayload
    val payloadRight = racerRight.suspendedPayload
    clearRacers()
    (Bits.makeRacedPair(completionLeft, completionRight): @switch) match
      case Bits.Raced_SS => endRaceWithSuccessBoth(payloadLeft, payloadRight)
      case Bits.Raced_FC | Bits.Raced_FS => endRaceWithFailure(payloadLeft)
      case Bits.Raced_SF | Bits.Raced_CF => endRaceWithFailure(payloadRight)
      case _ => endRaceWithCancelled()


  private def endRaceForOrPar(): Unit =
    val racerLeft = getLeftRacer
    val racerRight = getRightRacer
    val completionLeft = racerLeft.getCompletion
    val completionRight = racerRight.getCompletion
    val payloadLeft = racerLeft.suspendedPayload
    val payloadRight = racerRight.suspendedPayload
    clearRacers()
    (Bits.makeRacedPair(completionLeft, completionRight): @switch) match
      case Bits.Raced_SC => endRaceWithSuccessOne(payloadLeft)
      case Bits.Raced_CS => endRaceWithSuccessOne(payloadRight)
      case Bits.Raced_FC => endRaceWithFailure(payloadLeft)
      case Bits.Raced_CF => endRaceWithFailure(payloadRight)
      case _ => endRaceWithCancelled()


  private def endRaceForOrSeq(): Unit =
    val racerLeft = getLeftRacer
    val completionLeft = racerLeft.getCompletion
    val payloadLeft = racerLeft.suspendedPayload
    clearRacers()
    completionLeft match
      case Bits.Completion_Success => endRaceWithSuccessOne(payloadLeft)
      case Bits.Completion_Failure => endRaceWithFailure(payloadLeft)
      case Bits.Completion_Cancelled => endRaceWithSuccess2nd()


  private def endRaceWithSuccessBoth(payloadLeft: Any, payloadRight: Any): Unit =
    val comp = OpCascaded.zipAndRestart(
      stack = suspendedStack.nn,
      ftorLeft = payloadLeft,
      ftorRight = payloadRight,
      fun = suspendedPayload.asInstanceOf[(Any, Any) => Any]
    )
    suspendAsSuccessComp(comp)


  private def endRaceWithSuccessOne(payload: Any): Unit =
    val comp = OpCascaded.restart(
      stack = suspendedStack.nn,
      ftor = payload,
    )
    suspendAsSuccessComp(comp)


  private def endRaceWithSuccess2nd(): Unit =
    val comp = suspendedPayload.asInstanceOf[() => AnyComp]()
    suspendAsSuccessComp(comp)


  private def endRaceWithCancelled(): Unit =
    cancelBySelf()
    suspendAsCancelled()


  private def endRaceWithFailure(payload: Any): Unit =
    suspendAsFailure(payload.asInstanceOf[Cause])


  //-------------------------------------------------------------------
  // Suspend
  //-------------------------------------------------------------------


  def resume(): Unit =
    assert(isSuspended)
    suspendedStore.nn.getEnv.resumer.resume(this)


  private def isSuspended: Boolean = suspendedStack != null


  private def suspendInitial(comp: AnyComp, env: Env): Unit =
    suspendedTag     = comp.tag
    suspendedPayload = comp
    suspendedStep    = SC.Pop
    suspendedStack   = Stack.initial
    suspendedStore   = Store.initial(env)


  private def suspend(
    tag: Byte,
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
  ): Unit =
    assert(!isSuspended)
    suspendedTag     = tag
    suspendedPayload = payload
    suspendedStep    = step
    suspendedStack   = stack
    suspendedStore   = store


  private def clearSuspension(): Unit =
    suspendedTag     = 0
    suspendedPayload = null
    suspendedStep    = null
    suspendedStack   = null
    suspendedStore   = null


  private def suspendForRace(
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
  ): Unit =
    suspendedPayload = payload
    suspendedStep    = step
    suspendedStack   = stack
    suspendedStore   = store


  private def suspendAsSuccessPure(value: Any): Unit =
    suspendedTag = suspendedStep.nn.tag
    suspendedPayload = value


  private def suspendAsSuccessComp(comp: AnyComp): Unit =
    suspendedTag = comp.tag
    suspendedPayload = comp


  private def suspendAsCancelled(): Unit =
    suspendedTag = Step.Cancel.tag
    suspendedStep = Step.Cancel
    suspendedPayload = CancelPayload


  private def suspendAsFailure(cause: Cause): Unit =
    suspendedTag = Step.Throw.tag
    suspendedStep = Step.Throw
    suspendedPayload = cause


  //-------------------------------------------------------------------
  // Public API
  //-------------------------------------------------------------------


  override def toString: String = name


  override def name: String =
    if theName.isEmpty then
      theName = s"Fib#%04X".format(hashCode & 0xFFFF)
    theName


  override def parent: Fiber.Untyped | Warp =
    theParent match
      case fiber: FiberImpl => fiber.untyped
      case warp: WarpImpl => warp


  override def unsafeCancelAndForget(): Unit = doCancelAndForget()


  override def unsafeStatus(): Fiber.Status =
    var savedLeftLink: WaiterLink | Null = null
    var savedRightLink: WaiterLink | Null = null
    var savedVaryingBits: Byte = 0
    var savedOwnership: Byte = 0
    var savedPayload: Any = null
    synchronized {
      savedVaryingBits = varyingBits
      savedOwnership = theOwnership
      savedPayload = suspendedPayload
      savedLeftLink = prevWaiter
      savedRightLink = nextWaiter
    }
    if Bits.isPending(savedVaryingBits) then
      val role =
        def l = savedLeftLink.nn.asFiber
        def r = savedRightLink.nn.asFiber
        def w = savedPayload.asInstanceOf[Fiber.Untyped | Warp | OnceVar.Untyped]
        import Fiber.Role
        theOwnership match
          case Bits.Ownership_Self =>
            Bits.getArbiter(savedVaryingBits) match
              case Bits.Arbiter_None => if savedLeftLink == null then Role.Runner else Role.Standby
              case Bits.Arbiter_Left => Role.Arbiter(List(l))
              case Bits.Arbiter_Right => Role.Arbiter(List(r))
              case Bits.Arbiter_Both => Role.Arbiter(List(l, r))
          case Bits.Ownership_Waitee => Role.Waiter(w)
          case Bits.Ownership_Blocker => Role.Blocker
      val isCancelled = Bits.isCancellationSignalled(savedVaryingBits)
      Fiber.Status.Pending(role, isCancelled = isCancelled, isRacer = isRacer)
    else
      Fiber.Status.Completed(makeOutcome(void = true))


  override def unsafePoll(): Option[Zipper.Untyped] =
    var savedBits: Int = 0
    var savedPayload: Any = null
    synchronized {
      savedBits = varyingBits
      savedPayload = suspendedPayload
    }
    Bits.getCompletion(savedBits) match
      case Bits.Completion_Pending => None
      case completion => Some(getOrMakeZipper(savedPayload, completion))


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


  private def isRoot: Boolean = Bits.isRoot(constantBits)
  private def isExplicit: Boolean = Bits.isExplicit(constantBits)
  private def isReentry: Boolean = Bits.isReentry(constantBits)
  private def getCompletion: Int = Bits.getCompletion(varyingBits)
  private def getArbiterBits: Int = Bits.getArbiter(varyingBits)

  private def whichRacerAmI: Int = constantBits & Bits.Racer_Mask
  private def isRacer: Boolean = whichRacerAmI != Bits.Racer_None
  private def getArbiter: FiberImpl = theParent.asInstanceOf[FiberImpl]
  private def getLeftRacer: FiberImpl = prevWaiter.asInstanceOf[FiberImpl]
  private def getRightRacer: FiberImpl = nextWaiter.asInstanceOf[FiberImpl]
  private def clearRacers(): Unit = clearWaiterLink()
  private def setRacers(left: FiberImpl, right: FiberImpl | Null): Unit =
    prevWaiter = left
    nextWaiter = right


private[turbolift] object FiberImpl:
  type Callback = Outcome[Nothing] => Unit

  def create(comp: Computation[?, ?], resumer: Resumer, name: String, isReentry: Boolean, callback: Callback): FiberImpl =
    val reentryBit = if isReentry then Bits.Const_Reentry else 0
    val constantBits = (Bits.Tree_Root | reentryBit).toByte
    val warp = WarpImpl.initial()
    val fiber = new FiberImpl(constantBits, warp, name)
    warp.tryAddFiber(fiber)
    warp.initMain(fiber, callback.asInstanceOf[AnyCallback])
    val env = Env.initial(warp, resumer)
    fiber.suspendInitial(comp.untyped, env)
    fiber
