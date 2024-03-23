package turbolift.internals.engine
import scala.annotation.{tailrec, switch}
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import turbolift.{Computation, Signature}
import turbolift.io.{Fiber, Snap, Outcome, Cause, Exceptions}
import turbolift.interpreter.Control
import turbolift.internals.primitives.{Tags, ComputationCases => CC}
import turbolift.internals.executor.Executor
import turbolift.internals.engine.{StepCases => SC}
import Cause.{Cancelled => CancelPayload}
import FiberImpl.{InnerLoopResult, Become}


//// public for now, to make JOL work
/*private[turbolift]*/ final class FiberImpl private (
  private val constantBits: Byte,
  private var owner: Owner,
) extends Link with Fiber.Unsealed:
  private var varyingBits: Byte = 0
  private var suspendedTag: Byte = 0
  private var suspendedPayload: Any = null
  private var suspendedStep: Step | Null = null
  private var suspendedStack: Stack | Null = null
  private var suspendedStore: Store | Null = null
  private var suspendedEnv: Env | Null = null
  private var suspendedMark: Mark = Mark.none


  def run(): Halt =
    assert(isPending)
    assert(isSuspended)
    try
      outerLoop(suspendedEnv.nn.tickHigh)
    catch
      case e: Exceptions.Panic => endOfFiber(e)
      case e => endOfFiber(new Exceptions.Unhandled(e))


  @tailrec private def outerLoop(tickHigh: Short, lastTickLow: Short = 0, fresh: ControlImpl | Null = null): Halt =
    if tickHigh >= 0 then
      assert(isSuspended)

      val nextTickLow: Short =
        if lastTickLow == 0 then
          if cancellationCheck() then
            suspendAsCancelled()
          suspendedEnv.nn.tickLow
        else
          lastTickLow

      val result =
        val currentTag     = suspendedTag
        val currentPayload = suspendedPayload.nn
        val currentStep    = suspendedStep.nn
        val currentStack   = suspendedStack.nn
        val currentStore   = suspendedStore.nn
        val currentEnv     = suspendedEnv.nn
        val currentMark    = suspendedMark
        clearSuspension()
        innerLoop(
          tick     = nextTickLow,
          tag      = currentTag,
          payload  = currentPayload,
          step     = currentStep,
          stack    = currentStack,
          store    = currentStore,
          mark     = currentMark,
          env      = currentEnv,
          fresh    = if fresh != null then fresh else new ControlImpl,
        )

      result match
        case that: FiberImpl => that.outerLoop((tickHigh - 1).toShort)
        case Become(that, tickLow2, fresh) => that.outerLoop(tickHigh, tickLow2, fresh)
        case halt: Halt => halt
    else
      Halt.Yield(this)


  //===================================================================
  // Inner Loop
  //===================================================================


  @tailrec private def innerLoop(
    tick: Short,
    tag: Byte,
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
    env: Env,
    mark: Mark,
    fresh: ControlImpl,
  ): InnerLoopResult =
    if tick > 0 then
      val tick2 = (tick - 1).toShort

      inline def loopStep(
        payload: Any, step: Step, stack: Stack, store: Store,
        env: Env, mark: Mark, fresh: ControlImpl
      ): InnerLoopResult =
        innerLoop(tick2, step.tag, payload, step, stack, store, env, mark, fresh)

      inline def loopComp(
        comp: Computation[?, ?], step: Step, stack: Stack, store: Store,
        env: Env, mark: Mark, fresh: ControlImpl
      ): InnerLoopResult =
        innerLoop(tick2, comp.tag, comp, step, stack, store, env, mark, fresh)

      inline def loopCancel(stack: Stack, store: Store, env: Env, fresh: ControlImpl): InnerLoopResult =
        innerLoop(tick2, Tags.Step_Unwind, CancelPayload, Step.Cancel, stack, store, env, Mark.none, fresh)

      (tag: @switch) match
        case Tags.MapFlat =>
          val theMap = payload.asInstanceOf[CC.Map[Any, Any]]
          val comp2 = theMap.comp
          val step2 = SC.More(Tags.Step_MoreFlat, theMap.fun, step)
          loopComp(comp2, step2, stack, store, env, mark, fresh)

        case Tags.MapPure =>
          val theMap = payload.asInstanceOf[CC.Map[Any, Any]]
          val comp2 = theMap.comp
          val step2 = SC.More(Tags.Step_MorePure, theMap.fun, step)
          loopComp(comp2, step2, stack, store, env, mark, fresh)

        case Tags.Step_MoreFlat =>
          val theMore = step.asInstanceOf[SC.More]
          val step2 = theMore.next
          val comp2 = theMore.fun(payload).asInstanceOf[AnyComp]
          loopComp(comp2, theMore.next, stack, store, env, mark, fresh)

        case Tags.Step_MorePure =>
          val theMore = step.asInstanceOf[SC.More]
          val step2 = theMore.next
          val payload2 = theMore.fun(payload)
          loopStep(payload2, step2, stack, store, env, mark, fresh)

        case Tags.Perform =>
          val thePerform = payload.asInstanceOf[CC.Perform[Any, Any, Signature]]
          val (location, prompt) = stack.locateSignature(thePerform.sig)
          (thePerform.op(prompt.interpreter): @unchecked) match
            case comp: AnyComp => loopComp(comp, step, stack, store, env, mark, fresh)
            case fun =>
              mark.mustBeEmpty()
              val comp: AnyComp = (fun: @unchecked) match
                case fun: Function1[Control.Untyped, AnyComp] => fun(fresh)
                case fun: Function2[Control.Untyped, Any, AnyComp] => fun(fresh, store.getOrElseVoid(location))
              if comp.tag == Tags.Resume && prompt.features.isTailResump then
                val theResume = comp.asInstanceOf[CC.Resume[Any, Stan, Any]]
                val payload2 = theResume.value
                val store2 = store.setIfNotVoid(location, theResume.stan)
                loopStep(payload2, step, stack, store2, env, mark, fresh)
              else
                val step2 = stack.getStepAt(location)
                val mark2 = prompt.asMark
                fresh.init(stack, store, step, step2, prompt, location)
                loopComp(comp, step2, stack, store, env, mark2, new ControlImpl)

        case Tags.Pure =>
          val thePure = payload.asInstanceOf[CC.Pure[Any]]
          val payload2 = thePure.value
          loopStep(payload2, step, stack, store, env, mark, fresh)

        case Tags.Impure =>
          val theImpure = payload.asInstanceOf[CC.Impure[Any, Any]]
          val payload2 = theImpure.thunk()
          loopStep(payload2, step, stack, store, env, mark, fresh)

        case Tags.Resume =>
          val theResume = payload.asInstanceOf[CC.Resume[Any, Stan, Any]]
          val control = theResume.control.toImpl
          val step2 = control.step
          val payload2 = theResume.value
          val (stack2, store2) = OpSplice.spliceForResume(stack, store, step, control, mark, theResume.stan)
          loopStep(payload2, step2, stack2, store2, env, Mark.none, fresh)

        case Tags.Escape =>
          val theEscape = payload.asInstanceOf[CC.Escape[Any, Stan, Any]]
          val control = theEscape.control.toImpl
          val step2 = SC.Capture(control.prompt, aside = step, next = control.step)
          val comp2 = theEscape.body
          val (stack2, store2) = OpSplice.spliceForEscape(stack, store, step, control, mark, theEscape.stan)
          loopComp(comp2, step2, stack2, store2, env, Mark.none, fresh)

        case Tags.Local =>
          val theLocal = payload.asInstanceOf[CC.Local[Any, Stan, Any]]
          val control = theLocal.control.toImpl
          val comp2 = theLocal.body
          val (stack3, store3) =
            val step2 = SC.Capture(control.prompt, aside = step, next = control.step)
            val (stack2, store2) = OpSplice.spliceForLocal(stack, store, step, control, mark)
            val location = stack2.locatePrompt(control.prompt) //@#@OPTY already found
            OpPush.pushLocal(stack2, store2, step2, control.prompt, location, theLocal.stan, FrameKind.plain)
          loopComp(comp2, SC.Pop, stack3, store3, env, Mark.none, fresh)

        case Tags.Abort =>
          val theAbort = payload.asInstanceOf[CC.Abort[Any, Any]]
          val control = theAbort.control.toImpl
          val step2 = control.prompt.unwind
          val payload2 = theAbort.value
          val (stack2, store2) = OpSplice.spliceForAbort(stack, store, step, control, mark)
          loopStep(payload2, step2, stack2, store2, env, Mark.none, fresh)

        case Tags.Step_Capture =>
          OpSplit.forceSplitAndThen(stack, store, mark): (stack, store) =>
            val theCapture = step.asInstanceOf[SC.Capture]
            val prompt = theCapture.prompt
            val step2 = theCapture.aside
            val mark2 = prompt.asMark
            val payload2 =
              //@#@OPTY fuse
              val location = stack.locatePrompt(prompt)
              val stepMid = stack.getStepAt(location)
              val stan = store.getOrElseVoid(location)
              fresh.init(stack, store, theCapture.next, stepMid, prompt, location)
              (payload, fresh, stan)
            loopStep(payload2, step2, stack, store, env, mark2, new ControlImpl)

        case Tags.Step_Bridge =>
          OpSplit.forceSplitAndThen(stack, store, mark): (stack, store) =>
            val (stack2, store2, step2) = OpPush.drop(stack, store)
            loopStep(payload, step2, stack2, store2, env, Mark.none, fresh)

        case Tags.ZipPar =>
          val theZipPar = payload.asInstanceOf[CC.ZipPar[Any, Any, Any, Any]]
          //@#@TODO Too conservative? Should check for `features.isParallel` at `mark`, instead of at stack top
          if stack.head.features.isParallel && env.isParallelismRequested then
            val fiberLeft = new FiberImpl(Bits.ZipPar_Left, this)
            val fiberRight = new FiberImpl(Bits.ZipPar_Right, this)
            if tryStartRace(fiberLeft, fiberRight) then
              val (storeTmp, storeLeft) = OpCascaded.fork(stack, store)
              val (storeDown, storeRight) = OpCascaded.fork(stack, storeTmp)
              suspendForRace(theZipPar.fun, step, stack, storeDown, env, mark)
              val stack2 = stack.makeFork
              fiberRight.suspend(theZipPar.rhs.tag, theZipPar.rhs, SC.Pop, stack2, storeRight, env, Mark.none)
              fiberRight.resume()
              fiberLeft.innerLoop(tick2, theZipPar.lhs.tag, theZipPar.lhs, SC.Pop, stack2, storeLeft, env, Mark.none, fresh)
            else
              //// Must have been cancelled meanwhile
              loopCancel(stack, store, env, fresh)
          else
            //// Fallback to sequential
            val comp2 = CC.ZipSeq(theZipPar.lhs, () => theZipPar.rhs, theZipPar.fun)
            loopComp(comp2, step, stack, store, env, mark, fresh)

        case Tags.ZipSeq =>
          val theZipSeq = payload.asInstanceOf[CC.ZipSeq[Any, Any, Any, Any]]
          val step2 = SC.ZipSeqLeft(theZipSeq.rhsFun, theZipSeq.fun, step)
          val comp2 = theZipSeq.lhs
          loopComp(comp2, step2, stack, store, env, mark, fresh)

        case Tags.OrPar =>
          val theOrPar = payload.asInstanceOf[CC.OrPar[Any, Any]]
          if stack.head.features.isParallel && env.isParallelismRequested then
            val fiberLeft = new FiberImpl(Bits.OrPar_Left, this)
            val fiberRight = new FiberImpl(Bits.OrPar_Right, this)
            if tryStartRace(fiberLeft, fiberRight) then
              val (storeTmp, storeLeft) = OpCascaded.fork(stack, store)
              val (storeDown, storeRight) = OpCascaded.fork(stack, storeTmp)
              suspendForRace(null, step, stack, storeDown, env, mark)
              val stack2 = stack.makeFork
              fiberRight.suspend(theOrPar.rhs.tag, theOrPar.rhs, SC.Pop, stack2, storeRight, env, Mark.none)
              fiberRight.resume()
              fiberLeft.innerLoop(tick2, theOrPar.lhs.tag, theOrPar.lhs, SC.Pop, stack2, storeLeft, env, Mark.none, fresh)
            else
              //// Must have been cancelled meanwhile
              loopCancel(stack, store, env, fresh)
          else
            //// Fallback to sequential
            val comp2 = CC.OrSeq(theOrPar.lhs, () => theOrPar.rhs)
            loopComp(comp2, step, stack, store, env, mark, fresh)

        case Tags.OrSeq =>
          val theOrSeq = payload.asInstanceOf[CC.OrSeq[Any, Any]]
          val fiberLeft = new FiberImpl(Bits.OrSeq, this)
          if tryStartRaceOfOne(fiberLeft) then
            val (storeDown, storeLeft) = OpCascaded.fork(stack, store)
            suspendForRace(theOrSeq.rhsFun, step, stack, storeDown, env, mark)
            val stack2 = stack.makeFork
            fiberLeft.innerLoop(tick2, theOrSeq.lhs.tag, theOrSeq.lhs, SC.Pop, stack2, storeLeft, env, Mark.none, fresh)
          else
            //// Must have been cancelled meanwhile
            loopCancel(stack, store, env, fresh)

        case Tags.Step_ZipSeqLeft =>
          val theZipSeqLeft = step.asInstanceOf[SC.ZipSeqLeft]
          val comp2 = theZipSeqLeft.todoRight()
          val step2 = SC.ZipSeqRight(payload, theZipSeqLeft.fun, theZipSeqLeft.next)
          loopComp(comp2, step2, stack, store, env, mark, fresh)

        case Tags.Step_ZipSeqRight =>
          val theZipSeqRight = step.asInstanceOf[SC.ZipSeqRight]
          val step2 = theZipSeqRight.next
          val payload2 = theZipSeqRight.fun(theZipSeqRight.doneLeft, payload)
          loopStep(payload2, step2, stack, store, env, mark, fresh)

        case Tags.Handle =>
          OpSplit.forceSplitAndThen(stack, store, mark): (stack, store) =>
            val theHandle = payload.asInstanceOf[CC.Handle[Any, Any, [_] =>> Any, [_] =>> Any, Any, Any]]
            val interpreter = theHandle.handler.interpreter.untyped
            val prompt = new Prompt(interpreter)
            // if mark.nonEmpty then
            //   panic(s"Unsupported feature: handling local effect ${prompt} inside interpreter of ${mark.unwrap}.")
            for sig <- prompt.interpreter.signatures do
              if stack.containsSignature(sig) then
                panic(s"Unsupported feature: shadowing effect ${sig}.")
            val comp2 = interpreter.onInitial
            val step2 = new SC.Push(theHandle.body, prompt, step)
            loopComp(comp2, step2, stack, store, env, Mark.none, fresh)

        case Tags.Step_Push =>
          mark.mustBeEmpty()
          val thePush = step.asInstanceOf[SC.Push]
          val step2 = thePush.next
          val (stack2, store2) = OpPush.pushBase(stack, store, step2, thePush.prompt, payload.asStan)
          val comp2 = thePush.body
          loopComp(comp2, SC.Pop, stack2, store2, env, Mark.none, fresh)

        case Tags.Step_Pop =>
          OpSplit.forceSplitAndThen(stack, store, mark): (stack, store) =>
            if stack.canPop then
              val (stack2, store2, step2, prompt, frame, stan) = OpPush.pop(stack, store)
              if prompt.isRoot then
                if !frame.isGuard then
                  val env2 = frame.stan.asEnv
                  loopStep(payload, step2, stack2, store2, env2, Mark.none, fresh)
                else
                  val payload2 = Snap.Success(payload)
                  loopStep(payload2, step2, stack2, store2, env, Mark.none, fresh)
              else
                val comp2 = prompt.interpreter.onReturn(payload, stan)
                loopComp(comp2, step2, stack2, store2, env, Mark.none, fresh)
            else
              setResult(Bits.Completion_Success, payload)
              endOfFiber(tick2, fresh)

        case Tags.Step_Unwind =>
          mark.mustBeEmpty()
          val theUnwind = step.asInstanceOf[SC.Unwind]
          if stack.canPop then
            val (stack2, store2, step2, prompt, frame, stan) = OpPush.pop(stack, store)
            if prompt.isRoot then
              if !frame.isGuard then
                val env2 = frame.stan.asEnv
                loopStep(payload, step, stack2, store2, env2, Mark.none, fresh)
              else
                val payload2 =
                  if theUnwind.prompt == null then
                    if theUnwind.cancel then
                      Snap.Cancelled
                    else
                      Snap.Failure(payload.asInstanceOf[Cause])
                  else
                    Snap.Aborted(payload, theUnwind.prompt)
                loopStep(payload2, step2, stack2, store2, env, Mark.none, fresh)
            else
              val step3 = if prompt == theUnwind.prompt then step2 else step
              loopStep(payload, step3, stack2, store2, env, Mark.none, fresh)
          else
            val completionBits = if CancelPayload == payload then Bits.Completion_Cancelled else Bits.Completion_Failure
            setResult(completionBits, payload)
            endOfFiber(tick2, fresh)

        case _ => (tag: @switch) match
          case Tags.DoIO =>
            val theSideEffect = payload.asInstanceOf[CC.DoIO[Any, Any]]
            var result: Any = null
            var throwable = null.asInstanceOf[Throwable]
            try result = theSideEffect.thunk()
            catch case e: Throwable => throwable = e
            if throwable eq null then
              val payload2 = result
              loopStep(payload2, step, stack, store, env, mark, fresh)
            else
              val step2 = Step.Throw
              val payload2 = Cause(throwable)
              loopStep(payload2, step2, stack, store, env, Mark.none, fresh)

          case Tags.DoTry =>
            val theDoTry = payload.asInstanceOf[CC.DoTry[Any, Any]]
            var result: Any = null
            var throwable = null.asInstanceOf[Throwable]
            try result = theDoTry.thunk()
            catch case e: Throwable => throwable = e
            val payload2 = if throwable eq null then TrySuccess(result) else TryFailure(throwable)
            loopStep(payload2, step, stack, store, env, mark, fresh)

          case Tags.DoSnap =>
            OpSplit.forceSplitAndThen(stack, store, mark): (stack, store) =>
              val theDoSnap = payload.asInstanceOf[CC.DoSnap[Any, Any]]
              val location = stack.locatePrompt(env.prompt)
              val (stack2, store2) = OpPush.pushLocal(stack, store, step, env.prompt, location, env.asStan, FrameKind.guard)
              loopComp(theDoSnap.body, SC.Pop, stack2, store2, env, Mark.none, fresh)

          case Tags.Unsnap =>
            val theUnsnap = payload.asInstanceOf[CC.Unsnap[Any, Any]]
            //@#@TODO forbid uncancelling, it wouldnt work correctly anyway
            theUnsnap.snap match
              case Snap.Success(payload2) =>
                loopStep(payload2, step, stack, store, env, mark, fresh)
              case Snap.Failure(payload2) =>
                val step2 = Step.Throw
                loopStep(payload2, step2, stack, store, env, Mark.none, fresh)
              case theAborted: Snap.Aborted =>
                val payload2 = theAborted.value
                val step2 = theAborted.prompt.unwind
                loopStep(payload2, step2, stack, store, env, Mark.none, fresh)
              case Snap.Cancelled =>
                selfCancel()
                loopCancel(stack, store, env, fresh)

          case Tags.EnvAsk =>
            val theEnvAsk = payload.asInstanceOf[CC.EnvAsk[Any]]
            val value = theEnvAsk.fun(env)
            loopStep(value, step, stack, store, env, mark, fresh)

          case Tags.EnvMod =>
            val theEnvMod = payload.asInstanceOf[CC.EnvMod[Any, Any]]
            val env2 = theEnvMod.fun(env)
            if env2 eq env then
              loopComp(theEnvMod.body, step, stack, store, env, mark, fresh)
            else
              //@#@TODO avoid stack split, like in any other HOE
              OpSplit.forceSplitAndThen(stack, store, mark): (stack, store) =>
                val location = stack.locatePrompt(env.prompt)
                val (stack2, store2) = OpPush.pushLocal(stack, store, step, env.prompt, location, env2.asStan, FrameKind.plain)
                loopComp(theEnvMod.body, SC.Pop, stack2, store2, env2, Mark.none, fresh)

          case Tags.Yield =>
            suspend(step.tag, (), step, stack, store, env, mark)
            Halt.Yield(this)

    else
      suspend(tag, payload, step, stack, store, env, mark)
      this


  //===================================================================
  // End Of Fiber
  //===================================================================


  private def retire: Halt = Halt.retire(isReentry)


  private def endOfFiber(tick: Short, fresh: ControlImpl): InnerLoopResult =
    //@#@TODO more
    if isRoot then
      doFinalize()
      retire
    else
      if endRace() then
        Become(getArbiter, tick, fresh)
      else
        retire


  @tailrec private def endOfFiber(e: Throwable): Halt =
    if !isRoot then
      getArbiter.endOfFiber(e)
    else
      // println(e)
      // e.printStackTrace()
      setResultHard(Cause(e))
      doFinalize()
      retire


  //===================================================================
  // Finalization
  //===================================================================


  private def setResult(completionBits: Int, payload: Any): Unit =
    synchronized {
      //// If cancellation was sent before reaching completion, override the completion.
      val completionBits2 =
        if Bits.isCancellationUnreceived(varyingBits) then
          suspendedPayload = CancelPayload
          Bits.Completion_Cancelled
        else
          suspendedPayload = payload
          completionBits
      varyingBits = (varyingBits | completionBits2).toByte
    }


  private def setResultHard(cause: Cause): Unit =
    //// If cancellation was sent before reaching completion, ignore it.
    synchronized {
      varyingBits = (varyingBits | Bits.Completion_Failure).toByte
      suspendedPayload = cause
    }


  private def makeOutcome[A]: Outcome[A] =
    varyingBits & Bits.Completion_Mask match
      case Bits.Completion_Success   => Outcome.Success(suspendedPayload.asInstanceOf[A])
      case Bits.Completion_Failure   => Outcome.Failure(suspendedPayload.asInstanceOf[Cause])
      case Bits.Completion_Cancelled => Outcome.Cancelled


  def unsafeAwait[A](): Outcome[A] =
    synchronized {
      if isPending then
        wait()
    }
    makeOutcome


  private def doFinalize(): Unit =
    owner match
      case null => synchronized { notify() }
      case f: AnyCallback => f(makeOutcome)
      case _: FiberImpl => impossible


  //===================================================================
  // Cancelling
  //===================================================================


  private def cancellationCheck(): Boolean =
    synchronized {
      if Bits.isCancellationUnreceived(varyingBits) then
        varyingBits = (varyingBits | Bits.Cancellation_Received).toByte
        true
      else
        false
    }


  private def selfCancel(): Unit =
    synchronized {
      varyingBits = (varyingBits | Bits.Cancellation_Sent | Bits.Cancellation_Received).toByte
    }


  private def cancelRacerTree(): Unit =
    cancelLoop(this, Bits.Racer_None)


  @tailrec private def cancelLoop(limit: FiberImpl, comingFromRacer: Int): Unit =
    val nextToVisit: FiberImpl | Null = comingFromRacer match
      case Bits.Racer_None =>
        //// coming from parent
        synchronized {
          val bits = varyingBits
          if Bits.isPending(bits) && !Bits.isCancellationSent(bits) then
            varyingBits = (bits | Bits.Cancellation_Sent).toByte
            Bits.getRacer(bits) match
              case Bits.Racer_None => null
              case Bits.Racer_Right => getRightRacer
              case _ => getLeftRacer
          else
            null
        }
      case Bits.Racer_Left =>
        synchronized {
          if Bits.isPending(varyingBits) then
            Bits.getRacer(varyingBits) match
              case Bits.Racer_Right => getRightRacer
              case _ => null
          else
            null
        }
      case Bits.Racer_Right => null

    if nextToVisit != null then
      //// descent to first/next racer
      nextToVisit.cancelLoop(limit, Bits.Racer_None)
    else
      //// backtrack to arbiter
      if this != limit then
        getArbiter.cancelLoop(limit, whichRacerAmI)


  //===================================================================
  // Race
  //===================================================================

  
  //// Called by the ARBITER on itself
  private def tryStartRace(leftRacer: FiberImpl, rightRacer: FiberImpl): Boolean =
    tryStartRaceExt(leftRacer, rightRacer, Bits.Racer_Both)

  private def tryStartRaceOfOne(leftRacer: FiberImpl): Boolean =
    tryStartRaceExt(leftRacer, null, Bits.Racer_Left)

  private def tryStartRaceExt(leftRacer: FiberImpl, rightRacer: FiberImpl | Null, awaitingBits: Int): Boolean =
    val ok = 
      synchronized {
        //// Since we are synchronizing, let's check for Cancel signal too
        if Bits.isCancellationUnreceived(varyingBits) then
          varyingBits = (varyingBits | Bits.Cancellation_Received).toByte
          false
        else
          varyingBits = (varyingBits | awaitingBits).toByte
          true
      }
    if ok then
      setRacers(leftRacer, rightRacer)
    ok


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
            loser.cancelRacerTree()
          false
        else
          arbiter.endRaceForZipPar()
          true

      case Bits.Tree_OrPar =>
        if loser != null then
          if getCompletion != Bits.Completion_Cancelled then
            loser.cancelRacerTree()
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
      case _ => endRaceCancelled()


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
      case _ => endRaceCancelled()


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


  private def endRaceCancelled(): Unit =
    selfCancel()
    suspendAsCancelled()


  private def endRaceWithFailure(payload: Any): Unit =
    suspendAsFailure(payload.asInstanceOf[Cause])


  //===================================================================
  // Suspend
  //===================================================================


  private def isSuspended: Boolean = suspendedStack != null


  private def resume(): Unit =
    assert(isSuspended)
    suspendedEnv.nn.executor.resume(this)


  private def suspendInitial(comp: AnyComp, env: Env): Unit =
    suspendedTag     = comp.tag
    suspendedPayload = comp
    suspendedStep    = SC.Pop
    suspendedStack   = Stack.initial(env.prompt)
    suspendedStore   = Store.initial(env)
    suspendedEnv     = env
    suspendedMark    = Mark.none


  private def suspend(
    tag: Byte,
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
    env: Env,
    mark: Mark,
  ): Unit =
    assert(!isSuspended)
    suspendedTag     = tag
    suspendedPayload = payload
    suspendedStep    = step
    suspendedStack   = stack
    suspendedStore   = store
    suspendedEnv     = env
    suspendedMark    = mark


  private def clearSuspension(): Unit =
    suspendedTag     = 0
    suspendedPayload = null
    suspendedStep    = null
    suspendedStack   = null
    suspendedStore   = null
    suspendedEnv     = null
    suspendedMark    = Mark.none


  private def suspendForRace(
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
    env: Env,
    mark: Mark,
  ): Unit =
    suspendedPayload = payload
    suspendedStep    = step
    suspendedStack   = stack
    suspendedStore   = store
    suspendedEnv     = env
    suspendedMark    = mark


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
    suspendedMark = Mark.none


  private def suspendAsFailure(cause: Cause): Unit =
    suspendedTag = Step.Throw.tag
    suspendedStep = Step.Throw
    suspendedPayload = cause
    suspendedMark = Mark.none


  //===================================================================
  // Misc
  //===================================================================


  override def toString: String = s"Fiber#%08x".format(hashCode)

  def isReentry: Boolean = Bits.isReentry(constantBits)

  private def isPending: Boolean = Bits.isPending(varyingBits)
  private def isRoot: Boolean = Bits.isRoot(constantBits)
  private def getCompletion: Int = varyingBits & Bits.Completion_Mask

  private def whichRacerAmI: Int = constantBits & Bits.Racer_Mask
  private def getArbiter: FiberImpl = owner.asInstanceOf[FiberImpl]
  private def getLeftRacer: FiberImpl = linkLeft.asInstanceOf[FiberImpl]
  private def getRightRacer: FiberImpl = linkRight.asInstanceOf[FiberImpl]
  private def clearRacers(): Unit = clearLinks()
  private def setRacers(left: FiberImpl, right: FiberImpl | Null): Unit =
    linkLeft = left
    linkRight = right


private[internals] object FiberImpl:
  type Callback = Outcome[Nothing] => Unit

  private type InnerLoopResult = FiberImpl | Become | Halt
  private final case class Become(fiber: FiberImpl, tickLow: Short, fresh: ControlImpl)

  def create(comp: Computation[?, ?], executor: Executor, isReentry: Boolean, owner: Callback | Null = null): FiberImpl =
    val reentryBit = if isReentry then Bits.Const_Reentry else 0
    val constantBits = (Bits.Tree_Root | reentryBit).toByte
    val fiber = new FiberImpl(constantBits, owner.asInstanceOf[AnyCallback])
    val env = Env.default(executor = executor)
    fiber.suspendInitial(comp.untyped, env)
    fiber
