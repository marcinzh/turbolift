package turbolift.internals.engine
import turbolift.internals.interpreter.Control


private[engine] final class Kont(
  var prompt: Prompt,
  var step: Step,
  var stack: Stack,
  var store: Store,
) extends Control.Flow[Any, Any, Any, [X] =>> X, Any, Any]:

  def this() = this(
    prompt = null.asInstanceOf[Prompt],
    step = null.asInstanceOf[Step],
    stack = null.asInstanceOf[Stack],
    store = null.asInstanceOf[Store],
  )

  def init(prompt: Prompt, step: Step, stack: Stack, store: Store): Unit =
    this.prompt = prompt
    this.step = step
    this.stack = stack
    this.store = store

  def getStan: Any = store.getOrElseVoid(prompt)

  def getDivSegment: Segment = stack.getTopSegmentAt(prompt)
