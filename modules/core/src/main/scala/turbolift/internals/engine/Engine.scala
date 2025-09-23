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
import turbolift.internals.engine.concurrent.{OnceVarImpl, EffectfulVarImpl, CountDownLatchImpl, CyclicBarrierImpl, ReentrantLockImpl, SemaphoreImpl, ChannelImpl}
import Halt.{Retire => ThreadDisowned}
import Local.Syntax._
import Cause.{Cancelled => CancelPayload}
import Misc._


private trait Engine extends Runnable:
  this: FiberImpl =>


  final override def run(): Unit =
    runUntilYields() match
      case yielder: FiberImpl => yielder.resume()
      case _ => ()


  //// FiberImpl result means yield
  //// Boolean result means reentry
  final def runUntilYields(): FiberImpl | Boolean =
    this.theCurrentTickLow = theCurrentEnv.tickLow
    this.theCurrentTickHigh = theCurrentEnv.tickHigh
    this.theWaiteeOrBlocker = null //// for those resumed by `finallyResumeAllWaiters`
    if this.cancellationCheck() then
      this.suspendAsCancelled()
    outerLoop()


  //-------------------------------------------------------------------
  // Outer Loop
  //-------------------------------------------------------------------


  @tailrec private[engine] final def outerLoop(): FiberImpl | Boolean =
    dispatchNotify() //// Can modify this.suspendedTag/Step/Payload

    val halt =
      try
        middleLoop()
      catch e =>
        // e.printStackTrace()
        val e2 = if e.isInstanceOf[Exceptions.Panic] then e else new Exceptions.Unhandled(e)
        val c = Cause(e2)
        endOfLoop(Bits.Completion_Failure, c)

    halt match
      case Halt.Become =>
        val that = theFiberToBecome.nn
        this.theFiberToBecome = null
        that.outerLoop()

      case Halt.Yield => this

      case Halt.Retire => isReentry

      case _ => impossible //// handled at middleLoop


  //-------------------------------------------------------------------
  // Middle Loop
  //-------------------------------------------------------------------


  @tailrec private final def middleLoop(): Halt =
    val halt =
      (suspendedTag: @switch) match
        case (
          Tag.FlatMap | Tag.PureMap | Tag.MoreFlat | Tag.MorePure |
          Tag.Perform | Tag.Pure | Tag.Impure |
          Tag.LocalGet | Tag.LocalGetsEff | Tag.LocalPut | Tag.LocalModify | Tag.LocalUpdate | Tag.Sync
        ) =>
          val tag     = suspendedTag
          val payload = suspendedPayload
          val step    = suspendedStep.nn
          val stack   = suspendedStack.nn
          val store   = suspendedStore.nn
          this.suspendedTag = -1
          this.suspendedPayload = null
          this.suspendedStep = null
          this.suspendedStack = null
          this.suspendedStore = null
          innerLoop(tag, payload, step, stack, store, !theCurrentEnv.shadowMap.isEmpty)

        case Tag.Intrinsic =>
          val instr = suspendedPayload.asInstanceOf[CC.Intrinsic[Any, Any]]
          instr(this)

        case Tag.Unwind =>
          doUnwind()


    inline def doTickHigh(): Halt =
      if theCurrentTickHigh > 0 then
        theCurrentTickHigh -= 1
        theCurrentTickLow = theCurrentEnv.tickLow
        if this.cancellationCheck() then
          this.suspendAsCancelled()
        middleLoop()
      else
        Halt.Yield


    halt match
      case Halt.ContinueNoTick => middleLoop()

      case Halt.Continue =>
        if theCurrentTickLow > 0 then
          theCurrentTickLow -= 1
          middleLoop()
        else
          doTickHigh()

      case Halt.Reset => doTickHigh()

      case _ => halt


  //-------------------------------------------------------------------
  // Inner Loop
  //-------------------------------------------------------------------


  @annotation.nowarn("msg=already not null") // for cross compiling LTS & Next
  @tailrec private final def innerLoop(tag: Tag, payload: Any, step: Step, stack: Stack, store: Store, hasShadow: Boolean): Halt =
    inline def innerLoopStep(payload: Any, step: Step, store: Store): Halt =
      innerLoop(step.tag, payload, step, stack, store, hasShadow)

    inline def innerLoopComp(comp: Computation[?, ?], step: Step, store: Store): Halt =
      innerLoop(comp.tag, comp, step, stack, store, hasShadow)


    if theCurrentTickLow > 0 then
      theCurrentTickLow -= 1
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
          this.suspendedTag     = tag.toByte
          this.suspendedPayload = payload
          this.suspendedStep    = step
          this.suspendedStack   = stack
          this.suspendedStore   = store
          Halt.ContinueNoTick
    else
      this.suspendedTag     = tag.toByte
      this.suspendedPayload = payload
      this.suspendedStep    = step
      this.suspendedStack   = stack
      this.suspendedStore   = store
      Halt.Reset


  //-------------------------------------------------------------------
  // Loop Aux
  //-------------------------------------------------------------------


  private final def endOfLoop(completion: Int, payload: Any): Halt =
    val that = this.doFinalize(completion, payload)
    if that == null then
      Halt.Retire
    else
      become(that)


  private final def become(that: FiberImpl): Halt =
    theFiberToBecome = that
    that.theCurrentTickLow = theCurrentTickLow
    that.theCurrentTickHigh = theCurrentTickHigh
    Halt.Become


  private final def dispatchNotify(): Unit =
    (suspendedTag: @switch) match
      case Tag.NotifyOnceVar =>
        val ovar = suspendedPayload.asInstanceOf[OnceVarImpl]
        this.suspendedPayload = ovar.theContent
        this.suspendedTag = suspendedStep.nn.tag.toByte

      case Tag.NotifyEffectfulVar =>
        val evar = suspendedPayload.asInstanceOf[EffectfulVarImpl]
        val comp = evar.getNextShot
        this.suspendedPayload = comp
        this.suspendedTag = comp.tag.toByte

      case Tag.NotifyZipper =>
        val fiber = suspendedPayload.asInstanceOf[FiberImpl]
        this.suspendedPayload = fiber.getOrMakeZipper
        this.suspendedTag = suspendedStep.nn.tag.toByte

      case Tag.NotifyEither =>
        suspendedPayload.asInstanceOf[Either[Throwable, Any]] match
          case Right(a) =>
            this.suspendedPayload = a
            this.suspendedTag = suspendedStep.nn.tag.toByte
          case Left(e) =>
            this.suspendedPayload = Cause(e)
            this.suspendedTag = Step.Throw.tag.toByte
            this.suspendedStep = Step.Throw

      case _ => ()


  private final def doUnwind(): Halt =
    val payload = suspendedPayload
    val step    = suspendedStep.nn
    val stack   = suspendedStack.nn
    val store   = suspendedStore.nn
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
            this.suspendStep(payload, fallthrough, stack2, store2)
            val warp = theCurrentEnv.currentWarp.nn
            val tried = warp.exitMode match
              case Warp.ExitMode.Cancel => warp.tryGetCancelledBy(this)
              case Warp.ExitMode.Await => warp.tryGetAwaitedBy(this)
              case null => impossible //// this is a scoped warp, so it must have ExitMode
            tried match
              case Bits.WaiterSubscribed => ThreadDisowned
              case Bits.WaiterAlreadyCancelled => impossible //// Latch is set
              case Bits.WaiteeAlreadyCompleted =>
                this.clearSuspension()
                loopStepRefreshEnv(payload, fallthrough, stack2, store2)

          case FrameKind.EXEC =>
            refreshEnv(stack2, store2)
            this.suspendStep(payload, fallthrough, stack2, store2)
            this.resume()
            ThreadDisowned

          case FrameKind.SUPPRESS =>
            refreshEnv(stack2, store2)
            if this.cancellationCheck() then
              loopCancel(stack2, store2)
            else
              loopStep(payload, fallthrough, stack2, store2)
        end match
      else //// isIo
        if instr.isPop then
          val comp2 = prompt.onReturn(payload, local)
          loopComp(comp2, step2, stack2, store2)
        else
          if prompt == instr.prompt then
            loopStep(payload, step2, stack2, store2)
          else
            //@#@TODO reconcile nested unwinds
            val comp = prompt.onAbort(local).as(payload)
            loopComp(comp, step, stack2, store2)
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


  private final def loopStep(value: Any, step: Step, stack: Stack, store: Store): Halt =
    this.suspendedTag = step.tag.toByte
    this.suspendedPayload = value
    this.suspendedStep = step
    this.suspendedStack = stack
    this.suspendedStore = store
    Halt.Continue

  private final def loopComp(comp: !![?, ?], step: Step, stack: Stack, store: Store): Halt =
    this.suspendedTag = comp.tag.toByte
    this.suspendedPayload = comp
    this.suspendedStep = step
    this.suspendedStack = stack
    this.suspendedStore = store
    Halt.Continue

  private final def loopStepRefreshEnv(value: Any, step: Step, stack: Stack, store: Store): Halt =
    refreshEnv(stack, store)
    loopStep(value, step, stack, store)

  private final def loopCompRefreshEnv(comp: !![?, ?], step: Step, stack: Stack, store: Store): Halt =
    refreshEnv(stack, store)
    loopComp(comp, step, stack, store)

  private final def loopCancel(stack: Stack, store: Store): Halt =
    loopStep(CancelPayload, Step.Cancel, stack, store)


  //-------------------------------------------------------------------
  // Intrinsics
  //-------------------------------------------------------------------


  final def intrinsicDelimitPut[S](prompt: Prompt, body: AnyComp, local: S): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val location = stack.locatePrompt(prompt)
    val (stack2, store2) = OpPush.pushNested(stack, store, step, prompt, location, local.asLocal, FrameKind.plain)
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicDelimitMod[S](prompt: Prompt, body: AnyComp, fun: S => S): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val location = stack.locatePrompt(prompt)
    val local2 = fun.asInstanceOf[Local => Local](store.deepGet(location))
    val (stack2, store2) = OpPush.pushNested(stack, store, step, prompt, location, local2, FrameKind.plain)
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicAbort(prompt: Prompt, value: Any): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    loopStep(value, Step.abort(prompt), stack, store)


  final def intrinsicShadow[A, U](prompt: Prompt, body: A !! U): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val env2 = theCurrentEnv.copy(shadowMap = theCurrentEnv.shadowMap.push(prompt))
    val (stack2, store2) = OpPush.pushEnv(stack, store, step, env2)
    this.theCurrentEnv = env2
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicResume[A, B, S, U](cont0: Continuation[A, B, S, U], value: A): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
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


  final def intrinsicResumePut[A, B, S, U](cont0: Continuation[A, B, S, U], value: A, local: S): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
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


  final def intrinsicCapture[A, B, C, S, U, V](prompt: Prompt, fun: Continuation[A, B, S, U] => C !! V, truncate: Boolean): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val location = stack.locatePrompt(prompt)
    val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(stack, store, location, truncate)
    //@#@THOV only the shallow part of location2 is used, and only in `resumePut`
    //// `invalid` is safe bcoz `resumePut` can't be called on truncated continuation
    val location2 = if truncate then Location.Deep.invalid else stackHi.locatePrompt(prompt)
    val cont = new ContImpl(stackHi, storeHi, step, location2)
    val comp2 = fun(cont.cast[A, B, S, U])
    loopCompRefreshEnv(comp2, stepMid, stackLo, storeLo)


  final def intrinsicCaptureGet[A, B, C, S, U, V](prompt: Prompt, fun: (Continuation[A, B, S, U], S) => C !! V, truncate: Boolean): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
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


  final def intrinsicReinterpret[A, U, V](body: A !! (U & V)): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    //@#@TODO shadow map
    loopComp(body, step, stack, store)


  final def intrinsicZipPar[A, B, C, U](lhs: A !! U, rhs: B !! U, fun: (A, B) => C): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    if stack.accumFeatures.isParallel && theCurrentEnv.isParallelismRequested then
      val fiberLeft = this.createImplicit(Bits.ZipPar_Left)
      val fiberRight = this.createImplicit(Bits.ZipPar_Right)
      if this.tryStartRaceOfTwo(fiberLeft, fiberRight) then
        val stack2 = stack.lazyFork
        val (storeDown, storeLeft, storeRight) = OpCascaded.fork2(stack, store, stack2)
        this.suspendForRace(fun, step, stack, storeDown)
        fiberLeft.suspendComp(lhs, Step.Pop, stack2, storeLeft)
        fiberRight.suspendComp(rhs, Step.Pop, stack2, storeRight)
        fiberRight.resume()
        become(fiberLeft)
      else
        //// Must have been cancelled meanwhile
        loopCancel(stack, store)
    else
      //// Fallback to sequential
      val comp2 = lhs.zipWith(rhs)(fun)
      loopComp(comp2, step, stack, store)


  final def intrinsicOrPar[A, U](lhs: A !! U, rhs: A !! U): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    if stack.accumFeatures.isParallel && theCurrentEnv.isParallelismRequested then
      val fiberLeft = this.createImplicit(Bits.OrPar_Left)
      val fiberRight = this.createImplicit(Bits.OrPar_Right)
      if this.tryStartRaceOfTwo(fiberLeft, fiberRight) then
        val stack2 = stack.lazyFork
        val (storeDown, storeLeft, storeRight) = OpCascaded.fork2(stack, store, stack2)
        this.suspendForRace(null, step, stack, storeDown)
        fiberLeft.suspendComp(lhs, Step.Pop, stack2, storeLeft)
        fiberRight.suspendComp(rhs, Step.Pop, stack2, storeRight)
        fiberRight.resume()
        become(fiberLeft)
      else
        //// Must have been cancelled meanwhile
        loopCancel(stack, store)
    else
      //// Fallback to sequential
      val comp2 = lhs ||! rhs
      loopComp(comp2, step, stack, store)


  final def intrinsicOrSeq[A, U](lhs: A !! U, rhsFun: () => A !! U): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val fiberLeft = this.createImplicit(Bits.OrSeq)
    if this.tryStartRaceOfOne(fiberLeft) then
      val stack2 = stack.lazyFork
      val (storeDown, storeFork) = OpCascaded.fork1(stack, store, stack2)
      this.suspendForRace(rhsFun, step, stack, storeDown)
      fiberLeft.suspendComp(lhs, Step.Pop, stack2, storeFork)
      become(fiberLeft)
    else
      //// Must have been cancelled meanwhile
      loopCancel(stack, store)


  final def intrinsicHandle(body: AnyComp, prompt: Prompt, initial: Any): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    // for sig <- prompt.signatures do
    //   if stack.containsSignature(sig) then
    //     panic(s"Unsupported feature: shadowing effect ${sig}.")
    val (stack2, store2) = OpPush.pushBase(stack, store, step, prompt, initial.asLocal)
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicSnap[A, U](body: A !! U): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, theCurrentEnv, FrameKind.guard)
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicUnsnap[A, U](snap: Snap[A]): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    //@#@TODO forbid uncancelling, it wouldnt work correctly anyway
    (snap: @unchecked) match
      case Snap.Success(payload2)         => loopStep(payload2, step, stack, store)
      case Snap.Failure(payload2)         => loopStep(payload2, Step.Throw, stack, store)
      case Snap.Aborted(payload2, prompt) => loopStep(payload2, Step.abort(prompt), stack, store)
      case Snap.Cancelled =>
        //@#@THOV It should be harmless to self-cancel a fiber, even when it's uncancellable?
        this.cancelBySelf()
        loopCancel(stack, store)


  final def intrinsicEnvAsk[A](fun: Env => A): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val value = fun(theCurrentEnv)
    loopStep(value, step, stack, store)


  final def intrinsicEnvMod[A, U](fun: Env => Env, body: A !! U): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val env2 = fun(theCurrentEnv)
    if theCurrentEnv == env2 then
      loopComp(body, step, stack, store)
    else
      val (stack2, store2) = OpPush.pushEnv(stack, store, step, env2)
      this.theCurrentEnv = env2
      loopComp(body, Step.Pop, stack2, store2)



  final def intrinsicForkFiber[A, U](warp0: Warp | Null, comp: A !! U, name: String, callback: (Zipper.Untyped => Unit) | Null = null): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val warp = if warp0 != null then warp0.asImpl else theCurrentEnv.currentWarp.nn
    val stackFork = stack.lazyFork
    val (storeDown, storeFork) = OpCascaded.fork1(stack, store, stackFork)
    val child = FiberImpl.createExplicit(stackFork, warp, theCurrentEnv.fork, name, callback)
    child.suspendComp(comp, Step.Pop, stackFork, storeFork)
    if warp.tryAddFiber(child) then
      child.resume()
      loopStep(child, step, stack, storeDown)
    else
      child.suspendAsCancelled()
      loopStep(child, step, stack, store)


  final def intrinsicAwaitFiber[A, U](fiber: Fiber.Untyped, isCancel: Boolean, isVoid: Boolean): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val waitee = fiber.asImpl
    if waitee != this then
      if isVoid then
        this.suspendStep((), step, stack, store)
      else
        this.suspend(Tag.NotifyZipper, waitee, step, stack, store)
      val tried =
        if isCancel
        then waitee.tryGetCancelledBy(this)
        else waitee.tryGetAwaitedBy(this)
      tried match
        case Bits.WaiterSubscribed => ThreadDisowned
        case Bits.WaiterAlreadyCancelled =>
          this.clearSuspension()
          loopCancel(stack, store)
        case Bits.WaiteeAlreadyCompleted =>
          this.clearSuspension()
          val payload2 = if isVoid then () else waitee.getOrMakeZipper
          loopStep(payload2, step, stack, store)
    else
      //// Ignoring `isCancellable` bcoz cancelling is by-self
      if isCancel then
        this.cancelBySelf()
        loopCancel(stack, store)
      else
        val zombie = new Blocker.Zombie(this)
        this.suspendStep(null, step, stack, store)
        if this.tryGetBlocked(zombie) then
          ThreadDisowned
        else
          this.clearSuspension()
          loopCancel(stack, store)


  final def intrinsicCurrentFiber(): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    loopStep(this, step, stack, store)


  final def intrinsicSpawnWarp[A, U](exitMode: Warp.ExitMode, body: A !! (U & Warp), name: String): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val warp = new WarpImpl(this, theCurrentEnv.currentWarp, name, exitMode)
    val env2 = theCurrentEnv.copy(currentWarp = warp)
    val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2, FrameKind.warp)
    this.theCurrentEnv = env2
    loopComp(body, Step.Pop, stack2, store2)


  final def intrinsicAwaitWarp(warp0: Warp, isCancel: Boolean): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val warp = warp0.asImpl
    this.suspendStep((), step, stack, store)
    val tried =
      if isCancel
      then warp.tryGetCancelledBy(this)
      else warp.tryGetAwaitedBy(this)
    tried match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        this.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        this.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAsync[A](callback: (Either[Throwable, A] => Unit) => Unit, isAttempt: Boolean): Halt =
    //@#@TODO WTF non-Null & .toByte
    this.suspendedTag = if isAttempt then this.suspendedStep.nn.tag.toByte else Tag.NotifyEither
    this.suspendedPayload = null
    callback(this)
    ThreadDisowned


  final def intrinsicBlocking[A, B](thunk: () => A, isAttempt: Boolean): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val blocker = new Blocker.Interruptible(this, thunk, isAttempt)
    this.suspendStep(null, step, stack, store)
    if this.tryGetBlocked(blocker) then
      blocker.block()
      ThreadDisowned
    else
      this.clearSuspension()
      loopCancel(stack, store)


  final def intrinsicSleep(length: Long, unit: TimeUnit = TimeUnit.MILLISECONDS): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val blocker = new Blocker.Sleeper(this)
    this.suspendStep((), step, stack, store)
    if this.tryGetBlocked(blocker) then
      blocker.sleep(length, unit)
      ThreadDisowned
    else
      this.clearSuspension()
      loopCancel(stack, store)


  final def intrinsicSuppress[A, U](newValue: Boolean, body: Boolean => A !! U): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val oldValue = theCurrentEnv.isCancellable
    if oldValue == newValue then
      loopComp(body(oldValue), step, stack, store)
    else
      val env2 = theCurrentEnv.copy(isCancellable = newValue)
      val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2, FrameKind.suppress)
      this.theCurrentEnv = env2
      loopComp(body(oldValue), Step.Pop, stack2, store2)


  final def intrinsicExecOn[A, U](exec: Executor, body: A !! U): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    if theCurrentEnv.executor == exec then
      loopComp(body, step, stack, store)
    else
      val env2 = theCurrentEnv.copy(executor = exec)
      val (stack2, store2) = OpPush.pushNestedIO(stack, store, step, env2, FrameKind.exec)
      this.theCurrentEnv = env2
      this.suspendComp(body, Step.Pop, stack2, store2)
      this.resume()
      ThreadDisowned


  final def intrinsicYield: Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    this.suspendStep((), step, stack, store)
    Halt.Yield


  final def intrinsicAwaitOnceVar[A](ovar0: OnceVar.Get[A]): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val ovar = ovar0.asImpl
    val value = ovar.theContent
    if OnceVarImpl.Empty != value then
      loopStep(value, step, stack, store)
    else
      this.suspend(Tag.NotifyOnceVar, ovar, step, stack, store)
      ovar.tryGetAwaitedBy(this) match
        case Bits.WaiterSubscribed => ThreadDisowned
        case Bits.WaiterAlreadyCancelled =>
          this.clearSuspension()
          loopCancel(stack, store)
        case Bits.WaiteeAlreadyCompleted =>
          this.clearSuspension()
          val value = ovar.theContent
          loopStep(value, step, stack, store)


  final def intrinsicAwaitEffectfulVar[A, U <: IO](evar0: EffectfulVar.Get[A, U]): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    val evar = evar0.asImpl
    if evar.isReady then
      loopComp(evar.getNextShot, step, stack, store)
    else
      this.suspend(Tag.NotifyEffectfulVar, evar, step, stack, store)
      evar.tryGetAwaitedBy(this) match
        case Bits.WaiterSubscribed => ThreadDisowned
        case Bits.WaiterAlreadyCancelled =>
          this.clearSuspension()
          loopCancel(stack, store)
        case Bits.WaiteeAlreadyCompleted =>
          //@#@TODO temp solution in preparation for more rework
          val comp2 = this.suspendedPayload.asInstanceOf[AnyComp]
          this.clearSuspension()
          loopComp(comp2, step, stack, store)


  final def intrinsicAwaitCountDownLatch(latch: CountDownLatch): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    this.suspendStep((), step, stack, store)
    latch.asImpl.tryGetAwaitedBy(this) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        this.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        this.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAwaitCyclicBarrier(barrier: CyclicBarrier): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    this.suspendStep((), step, stack, store)
    barrier.asImpl.tryGetAwaitedBy(this) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        this.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        this.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAcquireReentrantLock(lock: ReentrantLock): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    this.suspendStep((), step, stack, store)
    lock.asImpl.tryGetAcquiredBy(this) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        this.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        this.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAcquireMutex(mutex: Mutex): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    this.suspendStep((), step, stack, store)
    mutex.asImpl.tryGetAcquiredBy(this) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        this.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        this.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicAcquireSemaphore(semaphore: Semaphore, count: Long): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    this.suspendStep((), step, stack, store)
    this.theWaiterStateLong = count
    semaphore.asImpl.tryGetAcquiredBy(this, count) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        this.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        this.clearSuspension()
        loopStep((), step, stack, store)


  final def intrinsicGetChannel[A](channel: Channel.Get[A]): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    this.suspendStep(null, step, stack, store)
    channel.asImpl.tryGetBy(this) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        this.clearSuspension()
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        //@#@TODO temp solution in preparation for more rework
        val value = this.suspendedPayload
        this.clearSuspension()
        loopStep(value, step, stack, store)


  final def intrinsicPutChannel[A](channel: Channel.Put[A], value: A): Halt =
    val step = suspendedStep.nn
    val stack = suspendedStack.nn
    val store = suspendedStore.nn
    //-------------------
    this.suspendStep((), step, stack, store)
    this.theWaiterStateAny = value
    channel.asImpl.tryPutBy(this) match
      case Bits.WaiterSubscribed => ThreadDisowned
      case Bits.WaiterAlreadyCancelled =>
        this.clearSuspension()
        this.theWaiterStateAny = null
        loopCancel(stack, store)
      case Bits.WaiteeAlreadyCompleted =>
        this.clearSuspension()
        this.theWaiterStateAny = null
        loopStep((), step, stack, store)


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


  private final def refreshEnv(stack: Stack, store: Store): Unit =
    this.theCurrentEnv = OpPush.findTopmostEnv(stack, store)


  private final def findEntryBySignature(sig: Signature, stack: Stack, hasShadow: Boolean): Entry =
    if !hasShadow then
      stack.findEntryBySignature(sig)
    else
      stack.findEntryBySignatureWithShadow(sig, theCurrentEnv.shadowMap.get(sig))


  private final def findEntryByPrompt(prompt: Prompt, stack: Stack, hasShadow: Boolean): Entry =
    if !hasShadow then
      stack.findEntryByPrompt(prompt)
    else
      stack.findEntryByPromptWithShadow(prompt, theCurrentEnv.shadowMap.get(prompt))
