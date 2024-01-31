package turbolift.internals.engine
import turbolift.interpreter.Control
import turbolift.Signature


private[engine] final class ControlImpl extends Control[Any, Any, Any, [X] =>> X, Any, Any]:
  var stack: Stack = null.asInstanceOf[Stack]
  var store: Store = null.asInstanceOf[Store]
  var step: Step = null.asInstanceOf[Step]
  var stepMid: Step = null.asInstanceOf[Step]
  var prompt: Prompt = null.asInstanceOf[Prompt]
  var location: Location.Deep = Location.Deep.empty
  var splits: (Stack, Store, Location.Deep) | Null = null


  def init(stack: Stack, store: Store, step: Step, stepMid: Step, prompt: Prompt, location: Location.Deep): Unit =
    this.stack = stack
    this.store = store
    this.step = step
    this.stepMid = stepMid
    this.prompt = prompt
    this.location = location


  //@#@TODO paranoid leak prevention: set original stack/store/step to null
  def forceSplitHi(): (Stack, Store, Location.Deep) =
    if splits != null then
      splits.nn
    else
      synchronized:
        if splits == null then
          //@#@OPTY fuse into splitHi
          val (stackHi, storeHi) = OpSplit.splitHi(stack, store, location)
          val locHi = stackHi.locatePrompt(prompt)
          splits = (stackHi, storeHi, locHi)
      splits.nn


extension (thiz: Control[?, ?, ?, ?, ?, ?])
  private[engine] def toImpl = thiz.asInstanceOf[ControlImpl]
