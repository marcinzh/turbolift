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
import Local.Syntax._
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
    this.theWaiteeOrBlocker = null //// for those resumed by `finallyResumeAllWaiters` or Blocker
    if this.cancellationCheck() then
      this.willContinueAsCancelled()
    outerLoop()


  //-------------------------------------------------------------------
  // Outer Loop
  //-------------------------------------------------------------------


  @tailrec private[engine] final def outerLoop(): FiberImpl | Boolean =
    dispatchNotify() //// Can modify this.theCurrentTag/Step/Payload

    val halt =
      try
        middleLoop()
      catch e =>
        // e.printStackTrace()
        val e2 = if e.isInstanceOf[Exceptions.Panic] then e else new Exceptions.Unhandled(e)
        this.theCurrentPayload = Cause(e2)
        endOfLoop(Bits.Completion_Failure)

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
      (theCurrentTag: @switch) match
        case (
          Tag.FlatMap | Tag.PureMap | Tag.MoreFlat | Tag.MorePure |
          Tag.Perform | Tag.Pure | Tag.Impure |
          Tag.LocalGet | Tag.LocalGetsEff | Tag.LocalPut | Tag.LocalModify | Tag.LocalUpdate | Tag.Sync
        ) =>
          val tag     = theCurrentTag
          val payload = theCurrentPayload
          val step    = theCurrentStep
          val stack   = theCurrentStack
          val store   = theCurrentStore
          this.theCurrentTag = -1
          this.theCurrentPayload = null
          this.theCurrentStep = null.asInstanceOf[Step]
          this.theCurrentStack = null.asInstanceOf[Stack]
          this.theCurrentStore = null.asInstanceOf[Store]
          innerLoop(tag, payload, step, stack, store, !theCurrentEnv.shadowMap.isEmpty)

        case Tag.Intrinsic =>
          val instr = theCurrentPayload.asInstanceOf[CC.Intrinsic[Any, Any]]
          instr(this)

        case Tag.Unwind =>
          doUnwind()


    inline def doTickHigh(): Halt =
      if theCurrentTickHigh > 0 then
        theCurrentTickHigh -= 1
        theCurrentTickLow = theCurrentEnv.tickLow
        if this.cancellationCheck() then
          this.willContinueAsCancelled()
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

      case Halt.Cancel =>
        //@#@TODO missing 1 tick
        this.willContinueAsCancelled()
        middleLoop()

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
          this.theCurrentTag     = tag.toByte
          this.theCurrentPayload = payload
          this.theCurrentStep    = step
          this.theCurrentStack   = stack
          this.theCurrentStore   = store
          Halt.ContinueNoTick
    else
      this.theCurrentTag     = tag.toByte
      this.theCurrentPayload = payload
      this.theCurrentStep    = step
      this.theCurrentStack   = stack
      this.theCurrentStore   = store
      Halt.Reset


  //-------------------------------------------------------------------
  // Loop Aux
  //-------------------------------------------------------------------


  private final def endOfLoop(completion: Int): Halt =
    val that = this.doFinalize(completion)
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
    (theCurrentTag: @switch) match
      case Tag.NotifyOnceVar =>
        val ovar = theCurrentPayload.asInstanceOf[OnceVarImpl]
        this.willContinuePure(ovar.theContent)

      case Tag.NotifyEffectfulVar =>
        val evar = theCurrentPayload.asInstanceOf[EffectfulVarImpl]
        this.willContinueEff(evar.getNextShot)

      case Tag.NotifyZipper =>
        val fiber = theCurrentPayload.asInstanceOf[FiberImpl]
        this.willContinuePure(fiber.getOrMakeZipper)

      case Tag.NotifyEither =>
        theCurrentPayload.asInstanceOf[Either[Throwable, Any]] match
          case Right(a) => this.willContinuePure(a)
          case Left(e) => this.willContinueAsFailure(e)

      case _ => ()


  private final def doUnwind(): Halt =
    val instr = theCurrentStep.asInstanceOf[Step.Unwind]
    if theCurrentStack.canPop then
      val (stack2, store2, step2, prompt, frame, local) = OpPush.pop(theCurrentStack, theCurrentStore)
      //// Keep unwinding, by default. Retains current Tag/Payload/Step. Overwritten in some branches.
      this.willContinueStack(stack2, store2)
      if prompt.isIo then
        val oldEnv = theCurrentEnv
        refreshEnv()
        //// Overwrite to stop unwinding.
        if instr.isPop then
          this.willContinueStep(step2)
        (frame.kind.unwrap: @switch) match
          case FrameKind.PLAIN => Halt.Continue

          case FrameKind.GUARD =>
            val snap = instr.kind match
              case Step.UnwindKind.Pop    => Snap.Success(theCurrentPayload)
              case Step.UnwindKind.Abort  => Snap.Aborted(theCurrentPayload, instr.prompt.nn)
              case Step.UnwindKind.Cancel => Snap.Cancelled
              case Step.UnwindKind.Throw  => Snap.Failure(theCurrentPayload.asInstanceOf[Cause])
            //// Overwrite to stop unwinding, regardless `isPop`.
            this.willContinuePureStep(snap, step2)
            Halt.Continue

          case FrameKind.WARP =>
            val warp = oldEnv.currentWarp.nn
            val tried = warp.exitMode match
              case Warp.ExitMode.Cancel => warp.tryGetCancelledBy(this)
              case Warp.ExitMode.Await => warp.tryGetAwaitedBy(this)
              case null => impossible //// this is a scoped warp, so it must have ExitMode
            tried match
              case Bits.WaiterSubscribed => Halt.Retire
              case Bits.WaiteeAlreadyCompleted => Halt.Continue
              case Bits.WaiterAlreadyCancelled => impossible //// Latch is set

          case FrameKind.EXEC =>
            this.resume()
            Halt.Retire

          case FrameKind.SUPPRESS =>
            if this.cancellationCheck() then
              Halt.Cancel
            else
              Halt.Continue
        end match
      else //// isIo
        if instr.isPop then
          val comp = prompt.onReturn(theCurrentPayload, local)
          this.willContinueEffStep(comp, step2)
          Halt.Continue
        else
          if prompt == instr.prompt then
            this.willContinueStep(step2)
            Halt.Continue
          else
            //@#@TODO reconcile nested unwinds
            val comp = prompt.onAbort(local).as(theCurrentPayload)
            this.willContinueEffStep(comp, instr)
            Halt.Continue
    else //// canPop
      val completion = instr.kind match
        case Step.UnwindKind.Pop    => Bits.Completion_Success
        case Step.UnwindKind.Cancel => Bits.Completion_Cancelled
        case Step.UnwindKind.Throw  => Bits.Completion_Failure
        case _                      => impossible
      endOfLoop(completion)


  //-------------------------------------------------------------------
  // Intrinsics
  //-------------------------------------------------------------------


  final def intrinsicDelimitPut[S](prompt: Prompt, body: AnyComp, local: S): Halt =
    val location = theCurrentStack.locatePrompt(prompt)
    val (stack2, store2) = OpPush.pushNested(theCurrentStack, theCurrentStore, theCurrentStep, prompt, location, local.asLocal, FrameKind.plain)
    this.willContinueEffStack(body, Step.Pop, stack2, store2)
    Halt.Continue


  final def intrinsicDelimitMod[S](prompt: Prompt, body: AnyComp, fun: S => S): Halt =
    val location = theCurrentStack.locatePrompt(prompt)
    val local2 = fun.asInstanceOf[Local => Local](theCurrentStore.deepGet(location))
    val (stack2, store2) = OpPush.pushNested(theCurrentStack, theCurrentStore, theCurrentStep, prompt, location, local2, FrameKind.plain)
    this.willContinueEffStack(body, Step.Pop, stack2, store2)
    Halt.Continue


  final def intrinsicAbort(prompt: Prompt, value: Any): Halt =
    this.willContinuePureStep(value, Step.abort(prompt))
    Halt.Continue


  final def intrinsicShadow[A, U](prompt: Prompt, body: A !! U): Halt =
    val env2 = theCurrentEnv.copy(shadowMap = theCurrentEnv.shadowMap.push(prompt))
    val (stack2, store2) = OpPush.pushEnv(theCurrentStack, theCurrentStore, theCurrentStep, env2)
    this.willContinueEffStackEnv(body, Step.Pop, stack2, store2, env2)
    Halt.Continue


  final def intrinsicResume[A, B, S, U](cont0: Continuation[A, B, S, U], value: A): Halt =
    val cont = cont0.asImpl
    val (step2, stack2, store2) = OpSplit.merge(
      stepHi  = cont.step,
      stackHi = cont.stack,
      storeHi = cont.store,
      stepLo  = theCurrentStep,
      stackLo = theCurrentStack,
      storeLo = theCurrentStore,
    )
    //// Order matters
    this.willContinuePureStack(value, step2, stack2, store2)
    refreshEnv()
    Halt.Continue


  final def intrinsicResumePut[A, B, S, U](cont0: Continuation[A, B, S, U], value: A, local: S): Halt =
    val cont = cont0.asImpl
    val (step2, stack2, store2) = OpSplit.merge(
      stepHi  = cont.step,
      stackHi = cont.stack,
      storeHi = cont.store.deepPutIfNotVoid(cont.location, local.asLocal),
      stepLo  = theCurrentStep,
      stackLo = theCurrentStack,
      storeLo = theCurrentStore,
    )
    //// Order matters
    this.willContinuePureStack(value, step2, stack2, store2)
    refreshEnv()
    Halt.Continue


  final def intrinsicCapture[A, B, C, S, U, V](prompt: Prompt, fun: Continuation[A, B, S, U] => C !! V, truncate: Boolean): Halt =
    val location = theCurrentStack.locatePrompt(prompt)
    val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(theCurrentStack, theCurrentStore, location, truncate)
    //@#@THOV only the shallow part of location2 is used, and only in `resumePut`
    //// `invalid` is safe bcoz `resumePut` can't be called on truncated continuation
    val location2 = if truncate then Location.Deep.invalid else stackHi.locatePrompt(prompt)
    val cont = new ContImpl(stackHi, storeHi, theCurrentStep, location2)
    val comp = fun(cont.cast[A, B, S, U])
    this.willContinueEffStack(comp, stepMid, stackLo, storeLo)
    refreshEnv()
    Halt.Continue


  final def intrinsicCaptureGet[A, B, C, S, U, V](prompt: Prompt, fun: (Continuation[A, B, S, U], S) => C !! V, truncate: Boolean): Halt =
    val location = theCurrentStack.locatePrompt(prompt)
    val local = theCurrentStore.deepGet(location)
    val (stackHi, storeHi, stepMid, stackLo, storeLo) = OpSplit.split(theCurrentStack, theCurrentStore, location, truncate)
    //@#@THOV only the shallow part of location2 is used, and only in `resumePut`
    //// `invalid` is safe bcoz `resumePut` can't be called on truncated continuation
    val location2 = if truncate then Location.Deep.invalid else stackHi.locatePrompt(prompt)
    val cont = new ContImpl(stackHi, storeHi, theCurrentStep, location2)
    val comp = fun(cont.cast[A, B, S, U], local.asInstanceOf[S])
    this.willContinueEffStack(comp, stepMid, stackLo, storeLo)
    refreshEnv()
    Halt.Continue


  final def intrinsicReinterpret[A, U, V](body: A !! (U & V)): Halt =
    //@#@TODO shadow map still experimental
    this.willContinueEff(body)
    Halt.Continue


  final def intrinsicZipPar[A, B, C, U](lhs: A !! U, rhs: B !! U, fun: (A, B) => C): Halt =
    if theCurrentStack.accumFeatures.isParallel && theCurrentEnv.isParallelismRequested then
      val fiberLeft = this.createImplicit(Bits.ZipPar_Left)
      val fiberRight = this.createImplicit(Bits.ZipPar_Right)
      if this.tryStartRaceOfTwo(fiberLeft, fiberRight) then
        val stack2 = theCurrentStack.lazyFork
        val (storeDown, storeLeft, storeRight) = OpCascaded.fork2(theCurrentStack, theCurrentStore, stack2)
        this.willContinuePureStore(fun, storeDown)
        fiberLeft.willContinueEffStack(lhs, Step.Pop, stack2, storeLeft)
        fiberRight.willContinueEffStack(rhs, Step.Pop, stack2, storeRight)
        fiberRight.resume()
        become(fiberLeft)
      else
        //// Must have been cancelled meanwhile
        Halt.Cancel
    else
      //// Fallback to sequential
      this.willContinueEff(lhs.zipWith(rhs)(fun))
      Halt.Continue


  final def intrinsicOrPar[A, U](lhs: A !! U, rhs: A !! U): Halt =
    if theCurrentStack.accumFeatures.isParallel && theCurrentEnv.isParallelismRequested then
      val fiberLeft = this.createImplicit(Bits.OrPar_Left)
      val fiberRight = this.createImplicit(Bits.OrPar_Right)
      if this.tryStartRaceOfTwo(fiberLeft, fiberRight) then
        val stack2 = theCurrentStack.lazyFork
        val (storeDown, storeLeft, storeRight) = OpCascaded.fork2(theCurrentStack, theCurrentStore, stack2)
        this.willContinuePureStore(null, storeDown)
        fiberLeft.willContinueEffStack(lhs, Step.Pop, stack2, storeLeft)
        fiberRight.willContinueEffStack(rhs, Step.Pop, stack2, storeRight)
        fiberRight.resume()
        become(fiberLeft)
      else
        //// Must have been cancelled meanwhile
        Halt.Cancel
    else
      //// Fallback to sequential
      this.willContinueEff(lhs ||! rhs)
      Halt.Continue


  final def intrinsicOrSeq[A, U](lhs: A !! U, rhsFun: () => A !! U): Halt =
    val fiberLeft = this.createImplicit(Bits.OrSeq)
    if this.tryStartRaceOfOne(fiberLeft) then
      val stack2 = theCurrentStack.lazyFork
      val (storeDown, storeFork) = OpCascaded.fork1(theCurrentStack, theCurrentStore, stack2)
      this.willContinuePureStore(rhsFun, storeDown)
      fiberLeft.willContinueEffStack(lhs, Step.Pop, stack2, storeFork)
      become(fiberLeft)
    else
      //// Must have been cancelled meanwhile
      Halt.Cancel


  final def intrinsicHandle(body: AnyComp, prompt: Prompt, initial: Any): Halt =
    // for sig <- prompt.signatures do
    //   if stack.containsSignature(sig) then
    //     panic(s"Unsupported feature: shadowing effect ${sig}.")
    val (stack2, store2) = OpPush.pushBase(theCurrentStack, theCurrentStore, theCurrentStep, prompt, initial.asLocal)
    this.willContinueEffStack(body, Step.Pop, stack2, store2)
    Halt.Continue


  final def intrinsicSnap[A, U](body: A !! U): Halt =
    val (stack2, store2) = OpPush.pushNestedIO(theCurrentStack, theCurrentStore, theCurrentStep, theCurrentEnv, FrameKind.guard)
    this.willContinueEffStack(body, Step.Pop, stack2, store2)
    Halt.Continue


  final def intrinsicUnsnap[A, U](snap: Snap[A]): Halt =
    (snap: @unchecked) match
      case Snap.Success(value) =>
        this.willContinuePure(value)
        Halt.Continue
      case Snap.Failure(value) =>
        this.willContinuePureStep(value, Step.Throw)
        Halt.Continue
      case Snap.Aborted(value, prompt) =>
        this.willContinuePureStep(value, Step.abort(prompt))
        Halt.Continue
      case Snap.Cancelled =>
        //@#@THOV It should be harmless to self-cancel a fiber, even when it's uncancellable?
        this.cancelBySelf()
        Halt.Cancel


  final def intrinsicEnvAsk[A](fun: Env => A): Halt =
    this.willContinuePure(fun(theCurrentEnv))
    Halt.Continue


  final def intrinsicEnvMod[A, U](fun: Env => Env, body: A !! U): Halt =
    val env2 = fun(theCurrentEnv)
    if env2 != theCurrentEnv then
      val (stack2, store2) = OpPush.pushEnv(theCurrentStack, theCurrentStore, theCurrentStep, env2)
      this.willContinueStackEnv(Step.Pop, stack2, store2, env2)
    this.willContinueEff(body)
    Halt.Continue


  final def intrinsicForkFiber[A, U](warp0: Warp | Null, comp: A !! U, name: String, callback: (Zipper.Untyped => Unit) | Null = null): Halt =
    val warp = if warp0 != null then warp0.asImpl else theCurrentEnv.currentWarp.nn
    val stackFork = theCurrentStack.lazyFork
    val (storeDown, storeFork) = OpCascaded.fork1(theCurrentStack, theCurrentStore, stackFork)
    val child = FiberImpl.createExplicit(stackFork, warp, theCurrentEnv.fork, name, callback)
    child.willContinueEffStack(comp, Step.Pop, stackFork, storeFork)
    this.willContinuePureStore(child, storeDown)
    if warp.tryAddFiber(child) then
      child.resume()
    else
      child.willContinueAsCancelled()
    Halt.Continue


  final def intrinsicAwaitFiber[A, U](fiber: Fiber.Untyped, isCancel: Boolean, isVoid: Boolean): Halt =
    val waitee = fiber.asImpl
    if waitee != this then
      if isVoid then
        this.willContinuePure(())
      else
        this.willContinueTag(Tag.NotifyZipper, waitee)
      val tried =
        if isCancel
        then waitee.tryGetCancelledBy(this)
        else waitee.tryGetAwaitedBy(this)
      tried match
        case Bits.WaiterSubscribed => Halt.Retire
        case Bits.WaiterAlreadyCancelled => Halt.Cancel
        case Bits.WaiteeAlreadyCompleted =>
          if !isVoid then
            this.willContinuePure(waitee.getOrMakeZipper)
          Halt.Continue
    else
      //// Ignoring `isCancellable` bcoz cancelling is by-self
      if isCancel then
        this.cancelBySelf()
        Halt.Cancel
      else
        val zombie = new Blocker.Zombie(this)
        this.willContinuePure(null)
        if this.tryGetBlocked(zombie) then
          Halt.Retire
        else
          Halt.Cancel


  final def intrinsicCurrentFiber(): Halt =
    this.willContinuePure(this)
    Halt.Continue


  final def intrinsicSpawnWarp[A, U](exitMode: Warp.ExitMode, body: A !! (U & Warp), name: String): Halt =
    val warp = new WarpImpl(this, theCurrentEnv.currentWarp, name, exitMode)
    val env2 = theCurrentEnv.copy(currentWarp = warp)
    val (stack2, store2) = OpPush.pushNestedIO(theCurrentStack, theCurrentStore, theCurrentStep, env2, FrameKind.warp)
    this.willContinueEffStackEnv(body, Step.Pop, stack2, store2, env2)
    Halt.Continue


  final def intrinsicAwaitWarp(warp0: Warp, isCancel: Boolean): Halt =
    this.willContinuePure(())
    val warp = warp0.asImpl
    val tried =
      if isCancel
      then warp.tryGetCancelledBy(this)
      else warp.tryGetAwaitedBy(this)
    tried match
      case Bits.WaiterSubscribed => Halt.Retire
      case Bits.WaiteeAlreadyCompleted => Halt.Continue
      case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicAsync[A](callback: (Either[Throwable, A] => Unit) => Unit, isAttempt: Boolean): Halt =
    this.willContinueTag(if isAttempt then this.theCurrentStep.tag else Tag.NotifyEither, null)
    callback(this)
    Halt.Retire


  final def intrinsicBlocking[A, B](thunk: () => A, isAttempt: Boolean): Halt =
    this.willContinuePure(null)
    val blocker = new Blocker.Interruptible(this, thunk, isAttempt)
    if this.tryGetBlocked(blocker) then
      blocker.block()
      Halt.Retire
    else
      Halt.Cancel


  final def intrinsicSleep(length: Long, unit: TimeUnit = TimeUnit.MILLISECONDS): Halt =
    this.willContinuePure(())
    val blocker = new Blocker.Sleeper(this)
    if this.tryGetBlocked(blocker) then
      blocker.sleep(length, unit)
      Halt.Retire
    else
      Halt.Cancel


  final def intrinsicSuppress[A, U](newValue: Boolean, body: Boolean => A !! U): Halt =
    val oldValue = theCurrentEnv.isCancellable
    if newValue != oldValue then
      val env2 = theCurrentEnv.copy(isCancellable = newValue)
      val (stack2, store2) = OpPush.pushNestedIO(theCurrentStack, theCurrentStore, theCurrentStep, env2, FrameKind.suppress)
      this.willContinueStackEnv(Step.Pop, stack2, store2, env2)
    this.willContinueEff(body(oldValue))
    Halt.Continue


  final def intrinsicExecOn[A, U](exec: Executor, body: A !! U): Halt =
    this.willContinueEff(body)
    if theCurrentEnv.executor == exec then
      Halt.Continue
    else
      val env2 = theCurrentEnv.copy(executor = exec)
      val (stack2, store2) = OpPush.pushNestedIO(theCurrentStack, theCurrentStore, theCurrentStep, env2, FrameKind.exec)
      this.willContinueStackEnv(Step.Pop, stack2, store2, env2)
      this.resume()
      Halt.Retire


  final def intrinsicYield: Halt =
    this.willContinuePure(())
    Halt.Yield


  final def intrinsicAwaitOnceVar[A](ovar0: OnceVar.Get[A]): Halt =
    val ovar = ovar0.asImpl
    val value = ovar.theContent
    if OnceVarImpl.Empty != value then
      this.willContinuePure(value)
      Halt.Continue
    else
      this.willContinueTag(Tag.NotifyOnceVar, ovar)
      ovar.tryGetAwaitedBy(this) match
        case Bits.WaiterSubscribed => Halt.Retire
        case Bits.WaiteeAlreadyCompleted =>
          this.willContinuePure(ovar.theContent)
          Halt.Continue
        case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicAwaitEffectfulVar[A, U <: IO](evar0: EffectfulVar.Get[A, U]): Halt =
    val evar = evar0.asImpl
    if evar.isReady then
      this.willContinueEff(evar.getNextShot)
      Halt.Continue
    else
      this.willContinueTag(Tag.NotifyEffectfulVar, evar)
      evar.tryGetAwaitedBy(this) match
        case Bits.WaiterSubscribed => Halt.Retire
        case Bits.WaiteeAlreadyCompleted => Halt.Continue
        case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicAwaitCountDownLatch(latch: CountDownLatch): Halt =
    this.willContinuePure(())
    latch.asImpl.tryGetAwaitedBy(this) match
      case Bits.WaiterSubscribed => Halt.Retire
      case Bits.WaiteeAlreadyCompleted => Halt.Continue
      case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicAwaitCyclicBarrier(barrier: CyclicBarrier): Halt =
    this.willContinuePure(())
    barrier.asImpl.tryGetAwaitedBy(this) match
      case Bits.WaiterSubscribed => Halt.Retire
      case Bits.WaiteeAlreadyCompleted => Halt.Continue
      case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicAcquireReentrantLock(lock: ReentrantLock): Halt =
    this.willContinuePure(())
    lock.asImpl.tryGetAcquiredBy(this) match
      case Bits.WaiterSubscribed => Halt.Retire
      case Bits.WaiteeAlreadyCompleted => Halt.Continue
      case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicAcquireMutex(mutex: Mutex): Halt =
    this.willContinuePure(())
    mutex.asImpl.tryGetAcquiredBy(this) match
      case Bits.WaiterSubscribed => Halt.Retire
      case Bits.WaiteeAlreadyCompleted => Halt.Continue
      case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicAcquireSemaphore(semaphore: Semaphore, count: Long): Halt =
    this.willContinuePure(())
    this.theWaiterStateLong = count
    semaphore.asImpl.tryGetAcquiredBy(this, count) match
      case Bits.WaiterSubscribed => Halt.Retire
      case Bits.WaiteeAlreadyCompleted => Halt.Continue
      case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicGetChannel[A](channel: Channel.Get[A]): Halt =
    this.willContinuePure(null)
    channel.asImpl.tryGetBy(this) match
      case Bits.WaiterSubscribed => Halt.Retire
      case Bits.WaiteeAlreadyCompleted => Halt.Continue
      case Bits.WaiterAlreadyCancelled => Halt.Cancel


  final def intrinsicPutChannel[A](channel: Channel.Put[A], value: A): Halt =
    this.willContinuePure(())
    this.theWaiterStateAny = value
    channel.asImpl.tryPutBy(this) match
      case Bits.WaiterSubscribed => Halt.Retire
      case Bits.WaiteeAlreadyCompleted =>
        this.theWaiterStateAny = null
        Halt.Continue
      case Bits.WaiterAlreadyCancelled =>
        this.theWaiterStateAny = null
        Halt.Cancel


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


  private final def refreshEnv(): Unit =
    this.theCurrentEnv = OpPush.findTopmostEnv(theCurrentStack, theCurrentStore)


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
