package turbolift.internals.engine
import turbolift.interpreter.Continuation


private[engine] final class ContImpl(
  val stack: Stack,
  val store: Store,
  val step: Step,
  val location: Location.Deep,
) extends Continuation[Any, Any, Any, Any]
