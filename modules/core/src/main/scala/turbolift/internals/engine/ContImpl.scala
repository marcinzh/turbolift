package turbolift.internals.engine
import turbolift.interpreter.Continuation
import turbolift.internals.engine.stacked.{Step, Stack, Store, Location}


private[engine] final class ContImpl(
  val stack: Stack,
  val store: Store,
  val step: Step,
  val location: Location.Deep,
) extends Continuation[Any, Any, Any, Any]
