package turbolift.internals.engine
import scala.annotation.{tailrec, switch}
import turbolift.{Computation, Signature}
import turbolift.io.{Fiber, Snap, Outcome, Cause, Exceptions}
import turbolift.interpreter.Control
import turbolift.internals.primitives.{Tags, ComputationCases => CC}
import turbolift.internals.engine.{StepCases => SC}
import Cause.{Cancelled => CancelPayload}


//// public for now, to make JOL work
/*private[turbolift]*/ final class FiberImpl private (
  private val constantBits: Byte,
  private var owner: Owner,
) extends FiberLink with Fiber.Unsealed:
  @volatile private var varyingBits: Int = 0
  private var suspendedTick: Short = 0
  private var suspendedTag: Byte = 0
  private var suspendedPayload: Any = null
  private var suspendedStep: Step | Null = null
  private var suspendedStack: Stack | Null = null
  private var suspendedStore: Store | Null = null
  private var suspendedEnv: Env | Null = null
  private var suspendedMark: Mark = Mark.none


  def this(comp: Computation[?, ?], env: Env) =
    this(Bits.Tree_Root.toByte, null)
    init(comp.untyped, env)


  def this(comp: Computation[?, ?], env: Env, callback: Outcome[?] => Unit) =
    this(Bits.Tree_Root.toByte, callback.asInstanceOf[AnyCallback])
    init(comp.untyped, env)


  private def init(comp: AnyComp, env: Env): Unit =
    suspend(
      tick    = env.tickLow,
      tag     = comp.tag,
      payload = comp,
      step    = SC.Pop,
      stack   = Stack.initial(env.prompt),
      store   = Store.initial(env),
      env     = env,
      mark    = Mark.none,
    )

  override def toString: String = s"Fiber#%08x".format(hashCode)

  def isSuspended: Boolean = suspendedStack != null


  private def suspend(
    tick: Short,
    tag: Byte,
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
    env: Env,
    mark: Mark,
  ): Unit =
    assert(!isSuspended)
    suspendedTick    = tick
    suspendedTag     = tag
    suspendedPayload = payload
    suspendedStep    = step
    suspendedStack   = stack
    suspendedStore   = store
    suspendedEnv     = env
    suspendedMark    = mark


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


  def submit(): Unit =
    assert(isSuspended)
    suspendedEnv.nn.executor.enqueue(this)


  def run(): FiberImpl =
    assert(isSuspended)
    suspendedTick = suspendedEnv.nn.tickLow
    try
      outerLoop(suspendedEnv.nn.tickHigh)
    catch e =>
      val e2 =
        if e.isInstanceOf[Exceptions.Panic] then
          e
        else
          new Exceptions.Unhandled(e)
      val that = findRootFiber
      that.setResultHard(Cause(e2))
      that


  @tailrec private def outerLoop(tickHigh: Short): FiberImpl =
    //// In `newBits`, `Cancellation_Received` means DIFFERENCE from last value
    //// not CURRENT value, as in `this.varyingBits`
    val newBits =
      synchronized {
        val oldBits = varyingBits
        if Bits.isCancellationUnreceived(oldBits) then
          val n = oldBits | Bits.Cancellation_Received
          varyingBits = n
          n
        else
          oldBits & ~Bits.Cancellation_Received
      }

    if Bits.isPending(newBits) && tickHigh > 0 then
      val tickHigh2: Short =
        if suspendedTick == 0 then
          suspendedTick = suspendedEnv.nn.tickLow
          (tickHigh - 1).toShort
        else
          tickHigh

      //// Must do it only once, when `Cancellation_Sent` is detected for the first time
      if Bits.isCancellationReceived(newBits) then
        val step = Step.Cancel
        suspendedTag = step.tag
        suspendedPayload = CancelPayload
        suspendedStep = step
      
      val that =
        val currentTick    = suspendedTick
        val currentTag     = suspendedTag
        val currentPayload = suspendedPayload.nn
        val currentStep    = suspendedStep.nn
        val currentStack   = suspendedStack.nn
        val currentStore   = suspendedStore.nn
        val currentEnv     = suspendedEnv.nn
        val currentMark    = suspendedMark
        
        suspendedTick    = 0
        suspendedTag     = 0
        suspendedPayload = null
        suspendedStep    = null
        suspendedStack   = null
        suspendedStore   = null
        suspendedEnv     = null
        suspendedMark    = Mark.none

        innerLoop(
          tick     = currentTick,
          tag      = currentTag,
          payload  = currentPayload,
          step     = currentStep,
          stack    = currentStack,
          store    = currentStore,
          mark     = currentMark,
          env      = currentEnv,
          fresh    = new ControlImpl,
        )

      if that.suspendedTick >= 0 then
        that.outerLoop(tickHigh2)
      else
        //// -1 means Yield
        that.suspendedTick = 0
        that
    else
      this


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
  ): FiberImpl =
    if tick > 0 then
      val tick2 = (tick - 1).toShort

      inline def loopStep(
        payload: Any, step: Step, stack: Stack, store: Store,
        env: Env, mark: Mark, fresh: ControlImpl
      ): FiberImpl =
        innerLoop(tick2, step.tag, payload, step, stack, store, env, mark, fresh)

      inline def loopComp(
        comp: Computation[?, ?], step: Step, stack: Stack, store: Store,
        env: Env, mark: Mark, fresh: ControlImpl
      ): FiberImpl =
        innerLoop(tick2, comp.tag, comp, step, stack, store, env, mark, fresh)

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
          var ok = true
          val payload2 =
            try
              theImpure.thunk()
            catch case e: Throwable =>
              ok = false
              Cause(e)
          if ok then
            loopStep(payload2, step, stack, store, env, mark, fresh)
          else
            val step2 = env.prompt.unwind
            loopStep(payload2, step2, stack, store, env, Mark.none, fresh)

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
            OpPush.pushLocal(stack2, store2, step2, control.prompt, location, theLocal.stan, guard = null)
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
              fiberRight.suspend(0, theZipPar.rhs.tag, theZipPar.rhs, SC.Pop, stack2, storeRight, env, Mark.none)
              fiberRight.submit()
              fiberLeft.innerLoop(tick2, theZipPar.lhs.tag, theZipPar.lhs, SC.Pop, stack2, storeLeft, env, Mark.none, fresh)
            else
              //// Must have been cancelled meanwhile
              loopStep(CancelPayload, Step.Cancel, stack, store, env, Mark.none, fresh)
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
              fiberRight.suspend(0, theOrPar.rhs.tag, theOrPar.rhs, SC.Pop, stack2, storeRight, env, Mark.none)
              fiberRight.submit()
              fiberLeft.innerLoop(tick2, theOrPar.lhs.tag, theOrPar.lhs, SC.Pop, stack2, storeLeft, env, Mark.none, fresh)
            else
              //// Must have been cancelled meanwhile
              loopStep(CancelPayload, Step.Cancel, stack, store, env, Mark.none, fresh)
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
            loopStep(CancelPayload, Step.Cancel, stack, store, env, Mark.none, fresh)

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
              val (stack2, store2) = OpPush.pushLocal(stack, store, step, env.prompt, location, env2.asStan, guard = null)
              loopComp(theEnvMod.body, SC.Pop, stack2, store2, env2, Mark.none, fresh)

        case Tags.DoSnap =>
          OpSplit.forceSplitAndThen(stack, store, mark): (stack, store) =>
            val theDoSnap = payload.asInstanceOf[CC.DoSnap[Any, Any, Any]]
            val location = stack.locatePrompt(env.prompt)
            val (stack2, store2) = OpPush.pushLocal(stack, store, step, env.prompt, location, env.asStan, theDoSnap.fun)
            loopComp(theDoSnap.body, SC.Pop, stack2, store2, env, Mark.none, fresh)

        case Tags.Unsnap =>
          val theUnsnap = payload.asInstanceOf[CC.Unsnap[Any]]
          //@#@TODO forbid uncancelling, it wouldnt work correctly anyway
          theUnsnap.snap match
            case Snap.Success(payload2) =>
              loopStep(payload2, step, stack, store, env, mark, fresh)
            case Snap.Failure(payload2) =>
              val step2 = env.prompt.unwind
              loopStep(payload2, step2, stack, store, env, Mark.none, fresh)
            case theAborted: Snap.Aborted =>
              val payload2 = theAborted.value
              val step2 = theAborted.prompt.unwind
              loopStep(payload2, step2, stack, store, env, Mark.none, fresh)
            case Snap.Cancelled =>
              selfCancel()
              val step2 = Step.Cancel
              val payload2 = CancelPayload
              loopStep(payload2, step2, stack, store, env, Mark.none, fresh)

        case Tags.Yield =>
          suspend(-1, step.tag, (), step, stack, store, env, mark)
          this

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
                if frame.guard == null then
                  val env2 = frame.stan.asEnv
                  loopStep(payload, step2, stack2, store2, env2, Mark.none, fresh)
                else
                  val snap = Snap.Success(payload)
                  val comp2 = frame.guard(snap)
                  loopComp(comp2, step2, stack2, store2, env, Mark.none, fresh)
              else
                val comp2 = prompt.interpreter.onReturn(payload, stan)
                loopComp(comp2, step2, stack2, store2, env, Mark.none, fresh)
            else
              val completionBits = setResult(Bits.Completion_Success, payload)
              endOfLoop(tick2, completionBits)

        case Tags.Step_Unwind =>
          mark.mustBeEmpty()
          val theUnwind = step.asInstanceOf[SC.Unwind]
          if stack.canPop then
            val (stack2, store2, step2, prompt, frame, stan) = OpPush.pop(stack, store)
            if prompt.isRoot then
              if frame.guard == null then
                val env2 = frame.stan.asEnv
                loopStep(payload, step, stack2, store2, env2, Mark.none, fresh)
              else
                val snap =
                  if theUnwind.prompt == null then
                    Snap.Cancelled
                  else if theUnwind.prompt.isRoot then
                    Snap.Failure(payload.asInstanceOf[Cause])
                  else
                    Snap.Aborted(payload, theUnwind.prompt)
                val comp2 = frame.guard(snap)
                loopComp(comp2, step2, stack2, store2, env, Mark.none, fresh)
            else
              val step3 = if prompt == theUnwind.prompt then step2 else step
              loopStep(payload, step3, stack2, store2, env, Mark.none, fresh)
          else
            val completionBits1 = if CancelPayload == payload then Bits.Completion_Cancelled else Bits.Completion_Failure
            val completionBits2 = setResult(completionBits1, payload)
            endOfLoop(tick2, completionBits2)

    else
      suspend(0, tag, payload, step, stack, store, env, mark)
      this


  //===================================================================
  // End Of Loop
  //===================================================================


  private def endOfLoop(tick: Short, completion: Int): FiberImpl =
    constantBits & Bits.Tree_Mask match 
      case Bits.Tree_Root => this

      case Bits.Tree_ZipPar =>
        val parent = getParentFiber
        val child = whichChildAmI
        if parent.tryWinRace(child) then
          if completion != Bits.Completion_Success then
            parent.cancelSiblingOf(child)
          this
        else
          parent.endRaceForZipPar(tick)

      case Bits.Tree_OrPar =>
        val parent = getParentFiber
        val child = whichChildAmI
        if parent.tryWinRace(child) then
          if completion != Bits.Completion_Cancelled then
            parent.cancelSiblingOf(child)
          this
        else
          parent.endRaceForOrPar(tick)

      case Bits.Tree_OrSeq =>
        val parent = getParentFiber
        parent.endRaceForOrSeq(tick)


  //===================================================================
  // Race
  //===================================================================

  
  //// Called by the PARENT
  private def tryStartRace(leftChild: FiberImpl, rightChild: FiberImpl): Boolean =
    tryStartRaceExt(leftChild, rightChild, Bits.Awaiting_Both)

  private def tryStartRaceOfOne(leftChild: FiberImpl): Boolean =
    tryStartRaceExt(leftChild, null, Bits.Awaiting_Left)

  private def tryStartRaceExt(leftChild: FiberImpl, rightChild: FiberImpl | Null, awaitingBits: Int): Boolean =
    val ok = 
      synchronized {
        val oldBits = varyingBits
        //// Since we are synchronizing, let's check for Cancel signal too
        if Bits.isCancellationUnreceived(oldBits) then
          varyingBits = oldBits | Bits.Cancellation_Received
          false
        else
          varyingBits = oldBits | awaitingBits
          true
      }
    if ok then
      linkLeft = leftChild
      linkRight = rightChild
    ok


  //// Called on the PARENT by its CHILD
  private def tryWinRace(childBit: Int): Boolean =
    val oldBits =
      synchronized {
        val n = varyingBits
        varyingBits = n & ~childBit
        n
      }
    (oldBits & Bits.Child_Mask) == Bits.Child_Both
  

  private def endRaceForZipPar(tick: Short): FiberImpl =
    val childLeft = getLeftChildFiber
    val childRight = getRightChildFiber
    val completionLeft = childLeft.getCompletion
    val completionRight = childRight.getCompletion
    val payloadLeft = childLeft.suspendedPayload
    val payloadRight = childRight.suspendedPayload
    clearLinks()
    suspendedTick = tick
    (Bits.makeRacedPair(completionLeft, completionRight): @switch) match
      case Bits.Raced_SS => endRaceWithSuccessBoth(payloadLeft, payloadRight)
      case Bits.Raced_FC | Bits.Raced_FS => endRaceWithFailure(payloadLeft)
      case Bits.Raced_SF | Bits.Raced_CF => endRaceWithFailure(payloadRight)
      case _ => endRaceCancelled


  private def endRaceForOrPar(tick: Short): FiberImpl =
    val childLeft = getLeftChildFiber
    val childRight = getRightChildFiber
    val completionLeft = childLeft.getCompletion
    val completionRight = childRight.getCompletion
    val payloadLeft = childLeft.suspendedPayload
    val payloadRight = childRight.suspendedPayload
    clearLinks()
    suspendedTick = tick
    (Bits.makeRacedPair(completionLeft, completionRight): @switch) match
      case Bits.Raced_SC => endRaceWithSuccessOne(payloadLeft)
      case Bits.Raced_CS => endRaceWithSuccessOne(payloadRight)
      case Bits.Raced_FC => endRaceWithFailure(payloadLeft)
      case Bits.Raced_CF => endRaceWithFailure(payloadRight)
      case _ => endRaceCancelled


  private def endRaceForOrSeq(tick: Short): FiberImpl =
    val childLeft = getLeftChildFiber
    val completionLeft = childLeft.getCompletion
    val payloadLeft = childLeft.suspendedPayload
    clearLinks()
    suspendedTick = tick
    completionLeft match
      case Bits.Completion_Success => endRaceWithSuccessOne(payloadLeft)
      case Bits.Completion_Failure => endRaceWithFailure(payloadLeft)
      case Bits.Completion_Cancelled => endRaceWithSuccess2nd


  private def endRaceWithSuccessBoth(payloadLeft: Any, payloadRight: Any): FiberImpl =
    val comp = OpCascaded.zipAndReintro(
      stack = suspendedStack.nn,
      ftorLeft = payloadLeft,
      ftorRight = payloadRight,
      fun = suspendedPayload.asInstanceOf[(Any, Any) => Any]
    )
    suspendedTag = comp.tag
    suspendedPayload = comp
    this


  private def endRaceWithSuccessOne(payload: Any): FiberImpl =
    val comp = OpCascaded.reintro(
      stack = suspendedStack.nn,
      ftor = payload,
    )
    suspendedTag = comp.tag
    suspendedPayload = comp
    this


  private def endRaceWithSuccess2nd: FiberImpl =
    val comp = suspendedPayload.asInstanceOf[() => AnyComp]()
    suspendedTag = comp.tag
    suspendedPayload = comp
    this


  private def endRaceCancelled: FiberImpl =
    selfCancel()
    suspendedTag = Step.Cancel.tag
    suspendedPayload = CancelPayload
    suspendedStep = Step.Cancel
    this


  private def endRaceWithFailure(payload: Any): FiberImpl =
    suspendedTag = Tags.Step_Unwind
    suspendedPayload = payload
    suspendedStep = suspendedEnv.nn.prompt.unwind
    this


  //===================================================================
  // Finalization
  //===================================================================


  private def setResult(completionBits: Int, payload: Any): Int =
    synchronized {
      val oldBits = varyingBits
      //// If cancellation was sent before reaching completion, override the completion.
      val completionBits2 =
        if Bits.isCancellationUnreceived(oldBits) then
          suspendedPayload = CancelPayload
          Bits.Completion_Cancelled
        else
          suspendedPayload = payload
          completionBits
      varyingBits = varyingBits | completionBits2
      completionBits2
    }


  private def setResultHard(cause: Cause): Unit =
    //// If cancellation was sent before reaching completion, ignore it.
    synchronized {
      varyingBits = varyingBits | Bits.Completion_Failure
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


  def doFinalize(): Unit =
    owner match
      case null => synchronized { notify() }
      case f: AnyCallback => f(makeOutcome)
      case _: FiberImpl => impossible


  //===================================================================
  // Cancelling
  //===================================================================


  private def selfCancel(): Unit =
    synchronized {
      varyingBits = varyingBits | Bits.Cancellation_Sent | Bits.Cancellation_Received
    }


  private def cancelSiblingOf(childBit: Int): Unit =
    getSiblingFiberOf(childBit).cull()


  private def cull(): Unit = cullRec(this, Bits.Child_None)

  @tailrec private def cullRec(limit: FiberImpl, comingFromChild: Int): Unit =
    val nextToVisit: FiberImpl | Null = comingFromChild match
      case Bits.Child_None =>
        //// coming from parent
        val awaitingBits =
          synchronized {
            val oldBits = varyingBits
            if Bits.isPending(oldBits) && !Bits.isCancellationSent(oldBits) then
              varyingBits = oldBits | Bits.Cancellation_Sent
              oldBits & Bits.Awaiting_Mask
            else
              Bits.Awaiting_None
          }
        awaitingBits match
          case Bits.Awaiting_None => null
          case Bits.Awaiting_Right => getRightChildFiber
          case _ => getLeftChildFiber

      case Bits.Child_Left =>
        varyingBits & Bits.Awaiting_Mask match
          case Bits.Awaiting_Right => getRightChildFiber
          case _ => null

      case Bits.Child_Right => null

    if nextToVisit != null then
      //// go to first/next child
      nextToVisit.cullRec(limit, Bits.Child_None)
    else
      //// go back to parent
      if this != limit then
        getParentFiber.cullRec(limit, whichChildAmI)


  //===================================================================
  // Misc
  //===================================================================


  private def getParentFiber: FiberImpl = owner.asInstanceOf[FiberImpl]
  private def getLeftChildFiber: FiberImpl = linkLeft.asInstanceOf[FiberImpl]
  private def getRightChildFiber: FiberImpl = linkRight.asInstanceOf[FiberImpl]
  private def getSiblingFiberOf(childBit: Int): FiberImpl = getChildFiber(childBit ^ Bits.Child_Mask)
  private def getSiblingFiber: FiberImpl | Null = getParentFiber.getSiblingFiberOf(whichChildAmI)

  private def findRootFiber: FiberImpl = if isRoot then this else getParentFiber.findRootFiber

  private def getChildFiber(childBit: Int): FiberImpl =
    childBit match
      case Bits.Child_Left => getLeftChildFiber
      case Bits.Child_Right => getRightChildFiber

  private def whichChildAmI: Int = constantBits & Bits.Child_Mask

  def isRoot: Boolean = Bits.isRoot(constantBits)
  def isPending: Boolean = Bits.isPending(varyingBits)
  def isSubstitute: Boolean = Bits.isSubstitute(varyingBits)
  
  def setSubstitute(): Unit =
    synchronized {
      varyingBits = varyingBits | Bits.Other_Substitute
    }
  
  private def getCompletion: Int = varyingBits & Bits.Completion_Mask

  private def isCancellationSent(): Boolean = Bits.isCancellationSent(varyingBits)
