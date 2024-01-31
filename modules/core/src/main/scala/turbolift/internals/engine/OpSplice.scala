package turbolift.internals.engine


private[engine] object OpSplice:
  private inline def forceSplitLo(stack: Stack, store: Store, step: Step, mark: Mark): (Stack, Store, Step) =
    val prompt = mark.unwrap
    if prompt != null then
      OpSplit.splitLo(stack, store, prompt)
    else
      (stack, store, step)


  private def isSameStackAs(stack: Stack, store: Store, control: ControlImpl): Boolean =
    (stack == control.stack) &&
    (store == control.store)


  def spliceForResume(
    stack: Stack,
    store: Store,
    step: Step,
    control: ControlImpl,
    mark: Mark,
    stan: Stan,
  ): (Stack, Store) =
    if isSameStackAs(stack, store, control) && (step eq control.stepMid) then
      val store2 = store.setIfNotVoid(control.location, stan)
      (stack, store2)
    else
      val (stackLo, storeLo, stepMid) = forceSplitLo(stack, store, step, mark)
      val (stackHi, storeHi, locHi) = control.forceSplitHi()
      val storeHi2 = storeHi.setIfNotVoid(locHi, stan)
      OpSplit.merge(
        stackHi = stackHi,
        storeHi = storeHi2,
        stepMid = step,
        stackLo = stackLo,
        storeLo = storeLo,
      )


  def spliceForEscape(
    stack: Stack,
    store: Store,
    step: Step,
    control: ControlImpl,
    mark: Mark,
    stan: Stan,
  ): (Stack, Store) =
    if isSameStackAs(stack, store, control) then
      val store2 = store.setIfNotVoid(control.location, stan)
      (stack, store2)
    else
      val (stackLo, storeLo, stepMid) = forceSplitLo(stack, store, step, mark)
      val (stackHi, storeHi, locHi) = control.forceSplitHi()
      val storeHi2 = storeHi.setIfNotVoid(locHi, stan)
      OpSplit.merge(
        stackHi = stackHi,
        storeHi = storeHi2,
        stepMid = stepMid,
        stackLo = stackLo,
        storeLo = storeLo,
      )


  def spliceForLocal(
    stack: Stack,
    store: Store,
    step: Step,
    control: ControlImpl,
    mark: Mark,
  ): (Stack, Store) =
    if isSameStackAs(stack, store, control) then
      (stack, store)
    else
      val (stackLo, storeLo, stepMid) = forceSplitLo(stack, store, step, mark)
      val (stackHi, storeHi, _) = control.forceSplitHi()
      OpSplit.merge(
        stackHi = stackHi,
        storeHi = storeHi,
        stepMid = stepMid,
        stackLo = stackLo,
        storeLo = storeLo,
      )


  def spliceForAbort(
    stack: Stack,
    store: Store,
    step: Step,
    control: ControlImpl,
    mark: Mark,
  ): (Stack, Store) =
    spliceForLocal(stack, store, step, control, mark)
