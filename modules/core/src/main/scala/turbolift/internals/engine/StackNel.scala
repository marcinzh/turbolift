package turbolift.internals.engine
import turbolift.interpreter.Features


private[engine] final class StackNel(
  override val head: StackSegment,
  val tail: Stack,
  val aside: Step,
) extends Stack:
  def asStack: Stack = this
  override val features: Features = head.features | tail.features
