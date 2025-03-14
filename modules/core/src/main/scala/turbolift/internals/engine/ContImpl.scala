package turbolift.internals.engine
import turbolift.interpreter.Continuation
import turbolift.internals.engine.stacked.{Step, Stack, Store, Location}


private[engine] final class ContImpl(
  val stack: Stack,
  val store: Store,
  val step: Step,
  //@#@TODO Redesign. Only the shallow part of `location` is used, and only
  //@#@TODO.. for overwriting Store during `resumePut`
  val location: Location.Deep,
) extends Continuation[Any, Any, Any, Any]
