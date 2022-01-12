package turbolift.abstraction.internals.engine
import turbolift.abstraction.internals.interpreter.Interpreter
import turbolift.abstraction.internals.interpreter.{InterpreterCases => IC}
import turbolift.abstraction.internals.interpreter.InverseControl


type EffectStack = Array[EffectStackItem]


sealed trait EffectStackItem:
  val interpreter: Interpreter


object EffectStackItem:
  final case class Proxy(override val interpreter: Interpreter) extends EffectStackItem

  final case class Flow(
    override val interpreter: Interpreter,
    val inverseControl: InverseControl,
  ) extends EffectStackItem

