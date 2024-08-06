package turbolift.internals.engine.concurrent
import scala.annotation.{tailrec, switch}
import turbolift.{Computation, Signature}
import turbolift.io.{Fiber, Zipper, Warp, OnceVar, Snap, Outcome, Cause, Exceptions}
import turbolift.internals.primitives.{Tags, ComputationCases => CC}
import turbolift.internals.executor.Executor
import turbolift.internals.engine.Env
import turbolift.internals.engine.Misc._
import turbolift.internals.engine.stacked.{StepCases, Step, Stack, Store, OpCascaded, OpPush}
import Cause.{Cancelled => CancelPayload}
import FiberImpl.Hook


private[turbolift] final class FiberImpl private (
  private[engine] val constantBits: Byte,
  private[engine] var theParent: WarpImpl | FiberImpl | Hook,
  private[engine] var theName: String,
) extends ChildLink with Fiber.Unsealed:
  private[engine] var suspendedTag: Byte = 0
  private[engine] var suspendedPayload: Any = null
  private[engine] var suspendedStep: Step | Null = null
  private[engine] var suspendedStack: Stack | Null = null
  private[engine] var suspendedStore: Store | Null = null

  private def this(constantBits: Byte, theName: String) = this(constantBits, null.asInstanceOf[Hook], theName)


  //-------------------------------------------------------------------
  // Finalization
  //-------------------------------------------------------------------


  private[engine] def doFinalize(completion: Int, initialPayload: Any, stack: Stack | Null): FiberImpl | Null =
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

    atomically {
      if isCancellationUnlatched then
        //// If cancellation was signalled before reaching completion, it overrides the completion.
        varyingBits = (varyingBits | Bits.Completion_Cancelled /*| Bits.Cancellation_Latch*/).toByte
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
          arbiter
        else
          null

      case warp: WarpImpl =>
        warp.removeFiber(this)
        if isRoot then
          warp.unsafeCancelAndForget()
        null

      case hook: Hook =>
        hook.warp.removeFiber(this)
        hook.call()
        null


  def makeOutcome[A]: Outcome[A] = makeOutcome(false)
  

  private def makeOutcome[A](void: Boolean): Outcome[A] =
    getCompletion match
      case Bits.Completion_Success   => Outcome.Success((if void then null else suspendedPayload).asInstanceOf[A])
      case Bits.Completion_Failure   => Outcome.Failure(suspendedPayload.asInstanceOf[Cause])
      case Bits.Completion_Cancelled => Outcome.Cancelled


  private[engine] def getOrMakeZipper: ZipperImpl =
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


  private[engine] def tryGetBlocked(isCancellable: Boolean): Boolean =
    atomicallyTry(isCancellable) {
      theOwnership = Bits.Ownership_Blocker
    }


  private[engine] def setBlockerUnsync(blocker: Blocker): Unit =
    suspendedPayload = blocker
    theOwnership = Bits.Ownership_Blocker


  private[engine] def tryGetAwaitedBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      if isPending then
        subscribeWaiterUnsync(waiter)
        Bits.WaiterSubscribed
      else
        Bits.WaiteeAlreadyCompleted
    }


  //-------------------------------------------------------------------
  // Cancelling
  //-------------------------------------------------------------------


  private[engine] def cancellationCheck(isCancellable: Boolean): Boolean =
    if isCancellable then
      !atomicallyTry(true) {}
    else
      false


  private[engine] def cancelBySelf(): Unit =
    atomically {
      varyingBits = (varyingBits | Bits.Cancellation_Signal | Bits.Cancellation_Latch).toByte
    }


  private def cancelByWinner(): Unit =
    deepCancelLoop(this)


  private[engine] def tryGetCancelledBy(canceller: FiberImpl, isCancellerCancellable: Boolean): Int =
    var savedLeftRacer: WaiterLink | Null = null
    var savedRightRacer: WaiterLink | Null = null
    var savedVaryingBits: Byte = 0
    var savedOwnership: Byte = 0
    var savedPayload: Any = null
    var willDescend = false

    val result =
      atomicallyBoth(canceller, isCancellerCancellable) {
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
  private[concurrent] override def deepCancelDown(): ChildLink | Null =
    var savedLeftRacer: WaiterLink | Null = null
    var savedRightRacer: WaiterLink | Null = null
    var savedVaryingBits: Byte = 0
    var savedOwnership: Byte = 0
    var savedPayload: Any = null

    val willDescend =
      atomically {
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


  private[concurrent] override def deepCancelRight(): ChildLink | Null =
    if whichRacerAmI == Bits.Racer_Left then
      getArbiter.tryGetRightRacer
    else
      //// Equals null for the right RACER, bcoz racer's parent is a fiber, not a warp.
      nextChild


  private def tryGetRightRacer: FiberImpl | Null =
    atomically {
      if isPending && (getArbiterBits == Bits.Arbiter_Right) then
        getRightRacer
      else
        null
    }


  private[concurrent] override def deepCancelUp(): ChildLink =
    theParent match
      case fiber: FiberImpl => fiber
      case warp: WarpImpl => warp
      case Hook(warp, _, _) => warp


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
  private[engine] def tryStartRace(leftRacer: FiberImpl, rightRacer: FiberImpl, isCancellable: Boolean): Boolean =
    tryStartRaceExt(leftRacer, rightRacer, isCancellable, Bits.Racer_Both)

  private[engine] def tryStartRaceOfOne(leftRacer: FiberImpl, isCancellable: Boolean): Boolean =
    tryStartRaceExt(leftRacer, null, isCancellable, Bits.Racer_Left)

  private def tryStartRaceExt(leftRacer: FiberImpl, rightRacer: FiberImpl | Null, isCancellable: Boolean, awaitingBits: Int): Boolean =
    atomicallyTry(isCancellable) {
      varyingBits = (varyingBits | awaitingBits).toByte
      setRacers(leftRacer, rightRacer)
    }


  //// Called by the RACER on its ARBITER
  private def tryWinRace(racerBit: Int): FiberImpl | Null =
    //// If win, return the loser
    atomically {
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
  // Suspend & Resume
  //-------------------------------------------------------------------


  def resume(): Unit =
    assert(isSuspended)
    val env = OpPush.findTopmostEnv(suspendedStack.nn, suspendedStore.nn)
    env.executor.resume(this)


  private def isSuspended: Boolean = suspendedStack != null


  private def suspendInitial(comp: AnyComp, env: Env): Unit =
    suspendedTag     = comp.tag
    suspendedPayload = comp
    suspendedStep    = StepCases.Pop
    suspendedStack   = Stack.initial
    suspendedStore   = Store.initial(env)


  private[engine] def suspend(
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


  private[engine] def clearSuspension(): Unit =
    suspendedTag     = 0
    suspendedPayload = null
    suspendedStep    = null
    suspendedStack   = null
    suspendedStore   = null


  private[engine] def suspendForRace(
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


  private[engine] def suspendAsCancelled(): Unit =
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
      case Hook(warp, _, _) => warp


  override def unsafeCancelAndForget(): Unit = doCancelAndForget()


  override def unsafeStatus(): Fiber.Status =
    var savedLeftLink: WaiterLink | Null = null
    var savedRightLink: WaiterLink | Null = null
    var savedVaryingBits: Byte = 0
    var savedOwnership: Byte = 0
    var savedPayload: Any = null
    atomically {
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
    atomically {
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
  private[internals] def isReentry: Boolean = Bits.isReentry(constantBits)
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

  def createChild(bits: Byte): FiberImpl = new FiberImpl(bits, this, "")


private[turbolift] object FiberImpl:
  type Callback = Outcome[Nothing] => Unit

  def create(comp: Computation[?, ?], executor: Executor, name: String, isReentry: Boolean, callback: Callback): FiberImpl =
    val reentryBit = if isReentry then Bits.Const_Reentry else 0
    val constantBits = (Bits.Tree_Root | reentryBit).toByte
    val fiber = new FiberImpl(constantBits, name)
    val warp = WarpImpl.root
    fiber.theParent = Hook(warp, fiber, callback.asInstanceOf[Any => Unit])
    warp.tryAddFiber(fiber)
    val env = Env.initial(executor)
    fiber.suspendInitial(comp.untyped, env)
    fiber


  def createExplicit(warp: WarpImpl, name: String, callback: (ZipperImpl => Unit) | Null): FiberImpl =
    val fiber = new FiberImpl(Bits.Tree_Explicit.toByte, name)
    fiber.theParent = if callback == null then warp else Hook(warp, fiber, callback.nn.asInstanceOf[Any => Unit])
    fiber


  final case class Hook(warp: WarpImpl, fiber: FiberImpl, callback: Any => Unit):
    def call(): Unit =
      if fiber.isExplicit then
        callback(fiber.suspendedPayload)
      else
        callback(fiber.makeOutcome)
