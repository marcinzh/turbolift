package turbolift.internals.engine
import scala.annotation.{tailrec, switch}
import turbolift.{!!, Signature}
import turbolift.internals.launcher.Callback
import turbolift.internals.interpreter.{Control, Void}
import turbolift.internals.primitives.{Tags, ComputationCases => CC}
import turbolift.internals.engine.{StepCases => SC, HistoryCases => HC}


private[engine] final class Fiber(
  _constantBits: Int, _parent: Fiber, _callback: Callback.Untyped
) extends FiberStub(_constantBits, _parent, _callback) with Runnable:

  def this(callback: Callback.Untyped) = this(Bits.Tree_Root, null.asInstanceOf[Fiber], callback)
  def this(parent: Fiber, constantBits: Int) = this(constantBits, parent, null.asInstanceOf[Callback.Untyped])

  private var suspendedTick: Int = 0
  private var suspendedTag: Int = 0
  private var suspendedPayload: Any = null
  private var suspendedStep: Step | Null = null
  private var suspendedStack: Stack | Null = null
  private var suspendedStore: Store | Null = null
  private var suspendedDiv: Boolean = false
  private def suspendedConfig = suspendedStack.nn.config

  def isSuspended: Boolean = suspendedStack != null

  def submit(): Unit =
    assert(isSuspended)
    suspendedConfig.nn.executor.execute(this)


  override def run(): Unit =
    assert(isSuspended)
    suspendedTick = suspendedConfig.nn.tickLow
    try
      outerLoop(suspendedConfig.nn.tickHigh)
    catch e =>
      findCallback.failure(
        if e.isInstanceOf[Panic]
        then e
        else
          val s = s"Unhandled exception: ${e.getMessage}"
          new Panic(s, e)
      )

        
  @tailrec private def outerLoop(tickHigh: Int): Unit =
    if unsafeIsCancelled() then
      suspendedTag = Tags.Step_Abort
      suspendedPayload = Cancelled
      suspendedStep = Prompt.global.abort

    val currentTick    = suspendedTick
    val currentTag     = suspendedTag
    val currentPayload = suspendedPayload.nn
    val currentStep    = suspendedStep.nn
    val currentStack   = suspendedStack.nn
    val currentStore   = suspendedStore.nn
    val currentDiv     = suspendedDiv
    
    suspendedTick    = 0
    suspendedTag     = 0
    suspendedPayload = null
    suspendedStep    = null
    suspendedStack   = null
    suspendedStore   = null
    suspendedDiv     = false

    val that = innerLoop(
      tick    = currentTick,
      tag     = currentTag,
      payload = currentPayload,
      step    = currentStep,
      stack   = currentStack,
      store   = currentStore,
      div     = currentDiv,
      kont0   = new Kont,
    )

    if that != null then
      if that.suspendedTick > 0 then
        that.outerLoop(tickHigh)
      else
        if tickHigh > 0 then
          that.suspendedTick = that.suspendedConfig.nn.tickLow
          that.outerLoop(tickHigh - 1)
        else
          that.submit()


  //===================================================================
  // Inner Loop {{
  //===================================================================

  @tailrec def innerLoop(
    tick: Int,
    tag: Int,
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
    div: Boolean,
    kont0: Kont,
  ): Fiber | Null =
    if tick > 0 then
      val tick2 = tick - 1
      (tag: @switch) match
        case Tags.MapFlat =>
          val theMap = payload.asInstanceOf[CC.Map[Any, Any]]
          val step2 = SC.More(Tags.Step_MoreFlat, theMap.fun, step)
          innerLoop(tick2, theMap.comp.tag, theMap.comp, step2, stack, store, div, kont0)

        case Tags.MapPure =>
          val theMap = payload.asInstanceOf[CC.Map[Any, Any]]
          val step2 = SC.More(Tags.Step_MorePure, theMap.fun, step)
          innerLoop(tick2, theMap.comp.tag, theMap.comp, step2, stack, store, div, kont0)

        case Tags.Perform =>
          // noDiv
          val thePerform = payload.asInstanceOf[CC.Perform[Any, Any, Signature]]
          val prompt = stack.lookup.find(thePerform.sig)
          val impl = thePerform.op(prompt.signature)
          if prompt.isFlow then
            val semantic =
              if prompt.hasStan then
                val stan = store.get(prompt)
                impl.asInstanceOf[(Control.Untyped, Any) => AnyComp](kont0, stan)
              else
                impl.asInstanceOf[Control.Untyped => AnyComp](kont0)
            if semantic.tag == Tags.Resume then
              val theResume = semantic.asInstanceOf[CC.Resume[Any, Any]]
              if theResume.ctrl eq kont0 then
                val store2 = store.setIfNotVoid(prompt, theResume.stan)
                innerLoop(tick2, step.tag, theResume.value, step, stack, store2, div, kont0)
              else
                kont0.init(prompt, step, stack, store)
                innerLoop(tick2, semantic.tag, semantic, prompt.abort, stack, store, true, new Kont)
            else
              kont0.init(prompt, step, stack, store)
              innerLoop(tick2, semantic.tag, semantic, prompt.abort, stack, store, true, new Kont)
          else
            val semantic = impl.asInstanceOf[AnyComp]
            innerLoop(tick2, semantic.tag, semantic, step, stack, store, div, kont0)

        case Tags.Step_MoreFlat =>
          val theMore = step.asInstanceOf[SC.More]
          val comp1 = theMore.fun(payload).asInstanceOf[AnyComp]
          innerLoop(tick2, comp1.tag, comp1, theMore.next, stack, store, div, kont0)

        case Tags.Step_MorePure =>
          val theMore = step.asInstanceOf[SC.More]
          val payload2 = theMore.fun(payload)
          innerLoop(tick2, theMore.next.tag, payload2, theMore.next, stack, store, div, kont0)

        case Tags.Pure =>
          val thePure = payload.asInstanceOf[CC.Pure[Any]]
          innerLoop(tick2, step.tag, thePure.value, step, stack, store, div, kont0)

        case Tags.Impure =>
          val theImpure = payload.asInstanceOf[CC.Impure[Any, Any]]
          var step2: Step = step
          val payload2 =
            try
              theImpure.thunk()
            catch
              case e: Throwable =>
                step2 = Prompt.global.abort
                e
          innerLoop(tick2, step2.tag, payload2, step2, stack, store, div, kont0)

        case _ => (tag: @switch) match
          case Tags.ZipWithPar =>
            val theZipWith = payload.asInstanceOf[CC.ZipWithPar[Any, Any, Any, Any]]
            if !div && stack.lookup.top.isParallelizable && stack.config.isParallelismRequested then
              val fiberLeft = new Fiber(this, Bits.Zip_Left)
              val fiberRight = new Fiber(this, Bits.Zip_Right)
              if this.makeChildrenAwaited(fiberLeft, fiberRight) then
                val (storeTmp, storeLeft) = stack.cascadedFork(store)
                val (storeDown, storeRight) = stack.cascadedFork(storeTmp)
                this.suspendedPayload = theZipWith.fun
                this.suspendedStep = step
                this.suspendedStack = stack
                this.suspendedStore = storeDown
                fiberRight.suspend(0, theZipWith.rhs.tag, theZipWith.rhs, SC.Done, stack.fork, storeRight, false)
                fiberRight.submit()
                fiberLeft.innerLoop(tick2, theZipWith.lhs.tag, theZipWith.lhs, SC.Done, stack.fork, storeLeft, false, kont0)
              else
                innerLoop(tick2, Tags.Step_Abort, Cancelled, Prompt.global.abort, stack, store, false, kont0)
            else
              val comp1 = theZipWith.lhs.flatMap(x => theZipWith.rhs.map(theZipWith.fun(x, _)))
              innerLoop(tick2, comp1.tag, comp1, step, stack, store, div, kont0)

          case Tags.Resume =>
            // yesDiv
            val theResume = payload.asInstanceOf[CC.Resume[Any, Any]]
            val ctrl = theResume.ctrl.asInstanceOf[Kont]
            val (stack2, store2) = stack.spliceForResume(step, ctrl, store, theResume.stan)
            innerLoop(tick2, ctrl.step.tag, theResume.value, ctrl.step, stack2, store2, false, kont0)

          case Tags.Local =>
            // yesDiv
            val theLocally = payload.asInstanceOf[CC.Local[Any, Any]]
            val ctrl = theLocally.ctrl.asInstanceOf[Kont]
            val (stack2, store2) = stack.spliceForLocal(step, ctrl, store, theLocally.stan)
            innerLoop(tick2, theLocally.body.tag, theLocally.body, SC.Done, stack2, store2, false, kont0)

          case Tags.Handle =>
            // noDiv
            if div then throw Panic("Adding local effect inside interpreter")
            val theHandle = payload.asInstanceOf[CC.Handle[Any, Any, [X] =>> Any, Any, Any]]
            val prompt = Prompt.create(stack.lookup.top, theHandle.handler.interpreter, store.nextIndex)
            if prompt.isFlow then
              val store2 = store.pushIfHasStan(prompt, theHandle.handler.initial)
              val stack2 = stack.pushFlow(prompt, step)
              innerLoop(tick2, theHandle.body.tag, theHandle.body, SC.Done, stack2, store2, false, kont0)
            else
              val stack2 = stack.pushProxy(prompt, step)
              innerLoop(tick2, theHandle.body.tag, theHandle.body, SC.Done, stack2, store, false, kont0)

          // ------------------------------------------

          case Tags.Step_Restore =>
            // noDiv
            val theRestore = step.asInstanceOf[SC.Restore]
            val (stack2, store2) = stack.spliceForRestore(theRestore.next, theRestore.kont, store)
            innerLoop(tick2, theRestore.aside.tag, payload, theRestore.aside, stack2, store2, true, kont0)

          case Tags.Step_Capture =>
            // noDiv
            val theCapture = step.asInstanceOf[SC.Capture]
            val prompt = theCapture.prompt
            val kont2 = new Kont(prompt, theCapture.next, stack, store)
            val payload2 = (payload, kont2)
            innerLoop(tick2, theCapture.aside.tag, payload2, theCapture.aside, stack, store, true, kont0)

          case Tags.Step_Done | Tags.Step_Abort =>
            innerLoop(tick2, stack.history.tag, payload, step, stack, store, false, kont0)

          // ------------------------------------------

          case Tags.History_Empty =>
            // noDiv
            if stack.canPopFlow then
              val (step2, stack2, store2, prompt, stan) = stack.popFlow(store)
              step.tag match
                case Tags.Step_Done =>
                  val payload2 = prompt.pure(payload, stan)
                  innerLoop(tick2, step2.tag, payload2, step2, stack2, store2, false, kont0)

                case Tags.Step_Abort =>
                  val theAbort = step.asInstanceOf[SC.Abort]
                  val step3 = if prompt == theAbort.prompt then step2 else step
                  innerLoop(tick2, step3.tag, payload, step3, stack2, store2, false, kont0)
            else
              this.constantBits & Bits.Tree_Mask match 
                case Bits.Tree_Root =>
                  step.tag match
                    case Tags.Step_Done =>
                      callback.success(payload)
                      null

                    case Tags.Step_Abort =>
                      assert(step.isGlobalAbort)
                      callback.failure(payload.asInstanceOf[Throwable])
                      null

                case Bits.Tree_Zip =>
                  this.suspendedPayload = payload
                  val bits = parent.tryWin(whichChildAmI, isSuccess = step.tag == Tags.Step_Done)
                  if (bits & Bits.Winner) != 0 then
                    null
                  else
                    //// I am the loser.
                    val successBits = bits & Bits.Child_Mask
                    if successBits == Bits.Child_Both then
                      val comp =
                        val ftorLeft  = parent.childLeft.nn.suspendedPayload
                        val ftorRight = parent.childRight.nn.suspendedPayload
                        parent.suspendedStack.nn.cascadedZipAndUnpure(
                          ftorLeft = ftorLeft,
                          ftorRight = ftorRight,
                          fun = parent.suspendedPayload.asInstanceOf[(Any, Any) => Any]
                        )
                      parent.clearChildren()
                      parent.suspendedTick = tick2
                      parent.suspendedTag = comp.tag
                      parent.suspendedPayload = comp
                      parent
                    else
                      val payload2 =
                        if successBits == Bits.Child_Left
                        then parent.childRight.nn.suspendedPayload
                        else parent.childLeft.nn.suspendedPayload
                      parent.clearChildren()
                      parent.suspendedTick = tick2
                      parent.suspendedTag = Tags.Step_Abort
                      parent.suspendedPayload = payload2
                      parent.suspendedStep = Prompt.global.abort
                      parent

          case Tags.History_Proxied =>
            // noDiv
            val h = stack.history.asInstanceOf[HC.Proxied]
            val step2 = step.doneOnce(h.step)
            val stack2 = stack.popProxy(h.savedLookup, h.next)
            innerLoop(tick2, step2.tag, payload, step2, stack2, store, false, kont0)

          case Tags.History_Configured =>
            // noDiv
            val h = stack.history.asInstanceOf[HC.Configured]
            val step2 = step.doneOnce(h.step)
            val stack2 = stack.popConfig(h.savedConfig, h.next)
            innerLoop(tick2, step2.tag, payload, step2, stack2, store, false, kont0)

          // ------------------------------------------

          case Tags.ConfigAsk =>
            val theConfigAsk = payload.asInstanceOf[CC.ConfigAsk[Any]]
            val value = theConfigAsk.fun(stack.config)
            innerLoop(tick2, step.tag, value, step, stack, store, div, kont0)

          case Tags.ConfigMod =>
            val theConfigMod = payload.asInstanceOf[CC.ConfigMod[Any, Any]]
            val config2 = theConfigMod.fun(stack.config)
            if config2 eq stack.config then
              innerLoop(tick2, theConfigMod.body.tag, theConfigMod.body, step, stack, store, div, kont0)
            else
              val stack2 = stack.pushConfig(config2, step)
              innerLoop(tick2, theConfigMod.body.tag, theConfigMod.body, SC.Done, stack2, store, div, kont0)
    else
      this.suspend(0, tag, payload, step, stack, store, div)
      this

  //===================================================================
  // Inner Loop }}
  //===================================================================

  private def suspend(
    tick: Int,
    tag: Int,
    payload: Any,
    step: Step,
    stack: Stack,
    store: Store,
    div: Boolean,
  ): Unit =
    assert(!isSuspended)
    suspendedTick    = tick
    suspendedTag     = tag
    suspendedPayload = payload
    suspendedStep    = step
    suspendedStack   = stack
    suspendedStore   = store
    suspendedDiv     = div

  private[this] def init(comp: AnyComp, config: Config): Unit =
    suspend(
      tick = config.tickLow,
      tag = comp.tag,
      payload = comp,
      step = StepCases.Done,
      stack = Stack.initial(config),
      store = Store.empty,
      div = false,
    )


private[internals] object Fiber:
  def makeRoot[A, U](comp: A !! U, config: Config, callback: Callback[A]): Fiber =
    val fiber = new Fiber(callback.untyped)
    fiber.init(comp.untyped, config)
    fiber
