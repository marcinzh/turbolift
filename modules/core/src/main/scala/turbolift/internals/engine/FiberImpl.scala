package turbolift.internals.engine
import scala.annotation.{tailrec, switch}
import turbolift.{Computation, Signature}
import turbolift.data.{Snap, Outcome, Cause, Exceptions}
import turbolift.io.{Fiber, Zipper, Warp, OnceVar}
import turbolift.internals.executor.Executor
import turbolift.internals.engine.Misc._
import turbolift.internals.engine.stacked.{Stack, Store}
import Cause.{Cancelled => CancelPayload}


private[turbolift] sealed abstract class FiberImplPart1 (
  _parent: ChildLink,
) extends ChildLink(_parent) with Fiber.Unsealed with Function1[Either[Throwable, Any], Unit]:
  private[engine] var theWaiteeOrBlocker: Waitee | Blocker | Null = null
  private[engine] var theWaiterStateAny: Any = null
  private[engine] var theWaiterStateLong: Long = 0
  private[engine] var theRacerId: Int = 0
  private[engine] val pad1_S1: Short = 0


private[turbolift] sealed abstract class FiberImplPart2 (
  _parent: ChildLink,
) extends FiberImplPart1(_parent):
  private[engine] var theCurrentTag: Byte = 0
  private[engine] var theCurrentPayload: Any = null
  private[engine] var theCurrentStep: Step = null.asInstanceOf[Step]
  private[engine] var theCurrentStack: Stack = null.asInstanceOf[Stack]
  private[engine] var theCurrentStore: Store = null.asInstanceOf[Store]
  private[engine] var theCurrentTickLow: Int = 0
  private[engine] var theCurrentEnv: Env = null.asInstanceOf[Env]


private[turbolift] final class FiberImpl private (
  _parent: ChildLink,
  private[engine] val constantBits: Byte,
  private[engine] var theName: String,
  private[engine] val theJoinStack: Stack | Null,
  private[engine] val theCallback: (Any => Unit) | Null,
) extends FiberImplPart2(_parent) with Engine:
  private[engine] var theCurrentTickHigh: Int = 0
  private[engine] var theFiberToBecome: FiberImpl | Null = null
  private[engine] val pad3_L1: Long = 0
  private[engine] val pad3_L2: Long = 0


  //-------------------------------------------------------------------
  // Finalization
  //-------------------------------------------------------------------


  private[engine] def doFinalize(completion: Int): FiberImpl | Null =
    if isExplicit then
      theCurrentPayload = ZipperImpl.make(theJoinStack, theCurrentPayload, completion)

    atomically {
      varyingBits = (varyingBits | completion).toByte
    }

    //// As a RACER:
    val isLastRacer = if isRacer then endRace() else false

    //// As a WAITEE:
    finallyNotifyAllWaiters()

    //// As a RACER or CHILD:
    val fiberToBecome =
      getParent match
        case arbiter: FiberImpl =>
          if isLastRacer then
            arbiter
          else
            null

        case warp: WarpImpl =>
          warp.removeFiber(this)
          //@#@THOV
          // if isRoot then
          //   extra.warp.unsafeCancelAndForget()
          null

        case _ => impossible


    //// Call the callback
    if theCallback != null then
      if isExplicit then
        theCallback(theCurrentPayload)
      else
        //// Must be the root fiber, bcoz implicit fibers dont get callbacks
        assert(isRoot)
        theCallback(makeOutcome)

    fiberToBecome



  def makeOutcome[A]: Outcome[A] = makeOutcome(false)
  

  private def makeOutcome[A](void: Boolean): Outcome[A] =
    getCompletion match
      case Bits.Completion_Success   => Outcome.Success((if void then null else theCurrentPayload).asInstanceOf[A])
      case Bits.Completion_Failure   => Outcome.Failure(theCurrentPayload.asInstanceOf[Cause])
      case Bits.Completion_Cancelled => Outcome.Cancelled


  private[engine] def getOrMakeZipper: ZipperImpl =
    if isExplicit then
      theCurrentPayload.asInstanceOf[ZipperImpl]
    else
      ZipperImpl.make(null, theCurrentPayload, getCompletion)


  private def getOrMakeZipper(payload: Any, completion: Int): ZipperImpl =
    if isExplicit then
      theCurrentPayload.asInstanceOf[ZipperImpl]
    else
      ZipperImpl.make(null, payload, completion)


  //-------------------------------------------------------------------
  // Awaiting
  //-------------------------------------------------------------------


  inline def atomicallyTry[A](inline body: => Unit): Boolean =
    val isCancellable = theCurrentEnv.isCancellable
    atomically {
      if isCancellable && isCancellationUnlatched then
        setCancellationLatch()
        false
      else
        body
        true
    }


  private[engine] def tryGetBlocked(blocker: Blocker): Boolean =
    atomicallyTry {
      theWaiteeOrBlocker = blocker
    }


  private[engine] def tryGetAwaitedBy(waiter: FiberImpl): Halt =
    atomicallyBoth(waiter) {
      if isPending then
        subscribeWaiterUnsync(waiter)
        Halt.Retire
      else
        Halt.Continue
    }


  //-------------------------------------------------------------------
  // Cancelling
  //-------------------------------------------------------------------


  private[engine] def cancellationCheck(): Boolean =
    if theCurrentEnv.isCancellable then
      atomically {
        if isCancellationUnlatched then
          setCancellationLatch()
          true
        else
          false
      }
    else
      false


  private[engine] def cancelBySelf(): Unit =
    atomically {
      varyingBits = (varyingBits | Bits.Cancellation_Signal | Bits.Cancellation_Latch).toByte
    }


  private def cancelByWinner(): Unit =
    deepCancelLoop(this)


  private[engine] def tryGetCancelledBy(canceller: FiberImpl): Halt =
    var savedFirstRacer: ChildLink | Null = null
    var savedWaiteeOrBlocker: Waitee | Blocker | Null = null
    var savedVaryingBits: Byte = 0
    var willDescend = false

    val halt =
      atomicallyBoth(canceller) {
        if isPending then
          if !isCancellationSignalled then
            varyingBits = (varyingBits | Bits.Cancellation_Signal).toByte
            willDescend = true
            savedFirstRacer = getFirstChild
            savedWaiteeOrBlocker = theWaiteeOrBlocker
            savedVaryingBits = varyingBits
          subscribeWaiterUnsync(canceller)
          Halt.Retire
        else
          Halt.Continue
      }

    if willDescend then
      val racer = doDescend(savedFirstRacer, savedWaiteeOrBlocker, savedVaryingBits)
      if racer != null then
        racer.deepCancelLoop(this)
    halt


  //// Same as `tryGetCancelledBy(canceller)`, except:
  //// - doesn't synchronize on the `canceller`
  //// - doesn't subscribe the `canceller`
  //// - doesn't initiate `deepCancelLoop`
  //// - returns first pending racer, instead of `Halt`
  private[engine] override def deepCancelDown(): ChildLink | Null =
    var savedFirstRacer: ChildLink | Null = null
    var savedWaiteeOrBlocker: Waitee | Blocker | Null = null
    var savedVaryingBits: Byte = 0

    val willDescend =
      atomically {
        if isPendingAndNotCancelled then
          varyingBits = (varyingBits | Bits.Cancellation_Signal).toByte
          savedFirstRacer = getFirstChild
          savedWaiteeOrBlocker = theWaiteeOrBlocker
          savedVaryingBits = varyingBits
          true
        else
          false
      }

    if willDescend then
      doDescend(savedFirstRacer, savedWaiteeOrBlocker, savedVaryingBits)
    else
      null


  private[engine] override def deepCancelRight(): ChildLink | Null =
    //@#@TEMP until rework of deepCancelLoop
    if isRacer then
      val x = getNextSibling
      if x != getArbiter.getFirstChild then
        x
      else
        null
    else
      getNextSibling


  private[engine] override def deepCancelUp(): ChildLink = getParent.nn


  private def doDescend(
    savedFirstRacer: ChildLink | Null,
    savedWaiteeOrBlocker: Waitee | Blocker | Null,
    savedVaryingBits: Byte,
  ): FiberImpl | Null =
    savedWaiteeOrBlocker match
      case null => savedFirstRacer.asInstanceOf[FiberImpl]

      case waitee: Waitee =>
        waitee.unsubscribeWaiter(this)
        null

      case blocker: Blocker =>
        blocker.unblock()
        null


  //-------------------------------------------------------------------
  // Race
  //-------------------------------------------------------------------


  //// Called by the ARBITER on itself
  private[engine] def tryStartRaceOfTwo(leftRacer: FiberImpl, rightRacer: FiberImpl): Boolean =
    atomicallyTry {
      this.theWaiterStateLong = 2
      emptyInsertTwoChildren(leftRacer, rightRacer)
    }


  //// Called by the ARBITER on itself
  private[engine] def tryStartRaceOfOne(racer: FiberImpl): Boolean =
    atomicallyTry {
      emptyInsertOneChild(racer)
    }


  //// Called by the RACER on itself, from `doFinalize`
  private def endRace(): Boolean =
    val arbiter = getArbiter
    constantBits & Bits.Tree_Mask match
      case Bits.Tree_RaceAll =>
        if getCompletion != Bits.Completion_Success then
          arbiter.tryWinRace(this)
        arbiter.tryFinishRace()

      case Bits.Tree_RaceFirst =>
        if getCompletion != Bits.Completion_Cancelled then
          arbiter.tryWinRace(this)
        arbiter.tryFinishRace()

      case Bits.Tree_RaceOther => true


  //// Called by the RACER on its ARBITER
  private def tryWinRace(racer: FiberImpl): Unit =
    val isWinner =
      atomically {
        if theWaiterStateAny == null then
          this.theWaiterStateAny = racer
          true
        else
          false
      }
    if isWinner then
      cancelAllLosers(racer)


  //// Called by the RACER on its ARBITER
  private def tryFinishRace(): Boolean =
    atomically {
      this.theWaiterStateLong -= 1
      theWaiterStateLong == 0
    }


  //// Called on the ARBITER
  private def cancelAllLosers(winner: FiberImpl): Unit =
    val limit = getFirstChild.asInstanceOf[FiberImpl]
    @tailrec def loop(racer: FiberImpl): Unit =
      if winner != racer then
        racer.cancelByWinner()
      val next = racer.getNextSibling
      if next != limit then
        loop(next.asInstanceOf[FiberImpl])
    loop(limit)


  //-------------------------------------------------------------------
  // Resume
  //-------------------------------------------------------------------


  //// `this` == callback for IO.async
  override def apply(ee: Either[Throwable, Any]): Unit =
    theCurrentPayload = ee
    // ee match
    //   case Right(a) => theCurrentPayload = a
    //   case Left(e) => suspendAsFailure(e)
    resume()


  def resume(): Unit = theCurrentEnv.executor.resume(this)


  //// Called only from `atomically` block.
  private[engine] inline def standbyWaiter(): Unit =
    theWaiteeOrBlocker = null


  //// Called only from `atomically` block.
  private[engine] inline def standbyWaiterPure(value: Any): Unit =
    theWaiteeOrBlocker = null
    theCurrentPayload = value


  //// Called only from `atomically` block.
  private[engine] inline def standbyWaiterComp(comp: AnyComp): Unit =
    theWaiteeOrBlocker = null
    theCurrentPayload = comp
    theCurrentTag = comp.tag.toByte


  //-------------------------------------------------------------------
  // Will Continue
  //-------------------------------------------------------------------


  final inline def willContinueTag(tag: Int, payload: Any): Unit =
    this.theCurrentTag = tag.toByte
    this.theCurrentPayload = payload


  final inline def willContinueTagStore(tag: Int, payload: Any, store: Store): Unit =
    this.theCurrentTag = tag.toByte
    this.theCurrentPayload = payload
    this.theCurrentStore = store


  final inline def willContinueStep(step: Step): Unit =
    this.theCurrentTag = step.tag.toByte
    this.theCurrentStep = step


  final inline def willContinueStepStack(step: Step, stack: Stack, store: Store): Unit =
    this.theCurrentTag = step.tag.toByte
    this.theCurrentStep = step
    this.theCurrentStack = stack
    this.theCurrentStore = store


  final inline def willContinuePure(value: Any): Unit =
    this.theCurrentTag = this.theCurrentStep.tag.toByte
    this.theCurrentPayload = value


  final inline def willContinuePureStore(value: Any, store: Store): Unit =
    this.theCurrentTag = this.theCurrentStep.tag.toByte
    this.theCurrentPayload = value
    this.theCurrentStore = store


  final inline def willContinuePureStep(value: Any, step: Step): Unit =
    this.theCurrentTag = step.tag.toByte
    this.theCurrentPayload = value
    this.theCurrentStep = step


  final inline def willContinuePureStack(value: Any, step: Step, stack: Stack, store: Store): Unit =
    this.theCurrentTag = step.tag.toByte
    this.theCurrentPayload = value
    this.theCurrentStep = step
    this.theCurrentStack = stack
    this.theCurrentStore = store


  final inline def willContinueEff(comp: AnyComp): Unit =
    this.theCurrentTag = comp.tag.toByte
    this.theCurrentPayload = comp


  final inline def willContinueEffStep(comp: AnyComp, step: Step): Unit =
    this.theCurrentTag = comp.tag.toByte
    this.theCurrentPayload = comp
    this.theCurrentStep = step


  final inline def willContinueEffStack(comp: AnyComp, step: Step, stack: Stack, store: Store): Unit =
    this.theCurrentTag = comp.tag.toByte
    this.theCurrentPayload = comp
    this.theCurrentStep = step
    this.theCurrentStack = stack
    this.theCurrentStore = store


  final inline def willContinueEffStackEnv(comp: AnyComp, step: Step, stack: Stack, store: Store, env: Env): Unit =
    this.theCurrentTag = comp.tag.toByte
    this.theCurrentPayload = comp
    this.theCurrentStep = step
    this.theCurrentStack = stack
    this.theCurrentStore = store
    this.theCurrentEnv = env


  final inline def willContinueStack(stack: Stack, store: Store): Unit =
    this.theCurrentStack = stack
    this.theCurrentStore = store


  final inline def willContinueStack(step: Step, stack: Stack, store: Store): Unit =
    this.theCurrentStep = step
    this.theCurrentStack = stack
    this.theCurrentStore = store


  final inline def willContinueStackEnv(step: Step, stack: Stack, store: Store, env: Env): Unit =
    this.theCurrentStep = step
    this.theCurrentStack = stack
    this.theCurrentStore = store
    this.theCurrentEnv = env


  final inline def willContinueAsCancelled(): Unit =
    this.theCurrentTag = Step.Cancel.tag.toByte
    this.theCurrentStep = Step.Cancel
    this.theCurrentPayload = CancelPayload


  final inline def willContinueAsFailure(cause: Cause): Unit =
    this.theCurrentTag = Step.Throw.tag.toByte
    this.theCurrentStep = Step.Throw
    this.theCurrentPayload = cause


  final inline def willContinueAsFailure(throwable: Throwable): Unit =
    willContinueAsFailure(Cause(throwable))


  //-------------------------------------------------------------------
  // Waiter State
  //-------------------------------------------------------------------


  private[engine] inline def takeWaiterState(): Any =
    val x = theWaiterStateAny
    theWaiterStateAny = null
    x


  private[engine] inline def takeWaiterStateAs[T](): T =
    val x = theWaiterStateAny.asInstanceOf[T]
    theWaiterStateAny = null
    x


  //-------------------------------------------------------------------
  // Public API
  //-------------------------------------------------------------------


  override def toString: String = name


  override def name: String =
    if theName.isEmpty then
      theName = s"Fib#%04X".format(hashCode & 0xFFFF)
    theName


  override def parent: Fiber.Untyped | Warp = getParent.asInstanceOf[Fiber.Untyped | Warp]


  override def unsafeCancelAndForget(): Unit = doCancelAndForget()


  override def unsafeStatus(): Fiber.Status =
    var savedFirstChild: ChildLink | Null = null
    var savedWaiteeOrBlocker: Waitee | Blocker | Null = null
    var savedVaryingBits: Byte = 0

    atomically {
      savedFirstChild = getFirstChild
      savedWaiteeOrBlocker = theWaiteeOrBlocker
      savedVaryingBits = varyingBits
    }

    if Bits.isPending(savedVaryingBits) then
      val role =
        def w = savedWaiteeOrBlocker.asInstanceOf[Fiber.Untyped | Warp | OnceVar.Untyped]
        import Fiber.Role
        savedWaiteeOrBlocker match
          case null =>
            if savedFirstChild == null then
              Role.Runner
            else
              //@#@TEMP must change API: Role.Arbiter(???)`
              val l = savedFirstChild.asInstanceOf[FiberImpl]
              if l.getNextSibling == l then
                Role.Arbiter(List(l))
              else
                val r = l.getNextSibling.asInstanceOf[FiberImpl]
                Role.Arbiter(List(l, r))
          case _: Waitee => Role.Waiter(w)
          case _: Blocker => Role.Blocker
      val isCancelled = Bits.isCancellationSignalled(savedVaryingBits)
      Fiber.Status.Pending(role, isCancelled = isCancelled, isRacer = isRacer)
    else
      Fiber.Status.Completed(makeOutcome(void = true))


  override def unsafePoll(): Option[Zipper.Untyped] =
    var savedBits: Int = 0
    var savedPayload: Any = null
    atomically {
      savedBits = varyingBits
      savedPayload = theCurrentPayload
    }
    Bits.getCompletion(savedBits) match
      case Bits.Completion_Pending => None
      case completion => Some(getOrMakeZipper(savedPayload, completion))


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


  private def isRoot: Boolean = Bits.isRoot(constantBits)
  private def isExplicit: Boolean = Bits.isExplicit(constantBits)
  private def isRacer: Boolean = Bits.isRacer(constantBits)
  private[internals] def isReentry: Boolean = Bits.isReentry(constantBits)
  private[engine] def getCompletion: Int = Bits.getCompletion(varyingBits)
  private[engine] def getArbiter: FiberImpl = getParent.asInstanceOf[FiberImpl]
  private[engine] def getLeftRacer: FiberImpl = getFirstChild.asInstanceOf[FiberImpl]
  private[engine] def getRightRacer: FiberImpl = getLeftRacer.getNextSibling.asInstanceOf[FiberImpl]

  def createImplicit(bits: Byte): FiberImpl =
    val that = new FiberImpl(
      _parent = this,
      constantBits = bits,
      theName = "",
      theJoinStack = null,
      theCallback = null,
    )
    that.theCurrentEnv = theCurrentEnv.fork
    that


private[turbolift] object FiberImpl:
  type Callback = Outcome[Nothing] => Unit

  def createRoot(comp: Computation[?, ?], executor: Executor, name: String, isReentry: Boolean, callback: Callback): FiberImpl =
    val reentryBit = if isReentry then Bits.Const_Reentry else 0
    val constantBits = (Bits.Tree_Root | reentryBit).toByte
    val env = Env.initial(executor)
    val fiber = new FiberImpl(
      _parent = WarpImpl.root,
      constantBits = constantBits,
      theName = name,
      theJoinStack = Stack.initial, //// can't be null, as long as callback takes Zipper
      theCallback = callback.asInstanceOf[(Any => Unit) | Null],
    )
    fiber.willContinueEffStackEnv(
      comp = comp.untyped,
      step = Step.Pop,
      stack = Stack.initial,
      store = Store.initial(env),
      env = env,
    )
    WarpImpl.root.tryAddFiber(fiber)
    fiber


  def createExplicit(joinStack: Stack, parentWarp: WarpImpl, env: Env, name: String, callback: (ZipperImpl => Unit) | Null): FiberImpl =
    val fiber = new FiberImpl(
      _parent = parentWarp,
      constantBits = Bits.Tree_Explicit.toByte,
      theName = name,
      theJoinStack = joinStack,
      theCallback = callback.asInstanceOf[(Any => Unit) | Null],
    )
    fiber.theCurrentEnv = env
    fiber
