package turbolift.internals.engine
import turbolift.internals.interpreter.Interpreter
import turbolift.internals.interpreter.{InterpreterCases => IC}
import turbolift.internals.interpreter.InverseControl


type EffectStack = Array[EffectStackItem]


sealed trait EffectStackItem:
  val interpreter: Interpreter


object EffectStackItem:
  final case class Proxy(override val interpreter: Interpreter) extends EffectStackItem

  final case class Flow(
    override val interpreter: Interpreter,
    val inverseControl: InverseControl,
  ) extends EffectStackItem

