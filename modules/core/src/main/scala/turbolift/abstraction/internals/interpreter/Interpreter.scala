package turbolift.abstraction.internals.interpreter
import turbolift.abstraction.internals.effect.HasEffectId


sealed trait Interpreter extends HasEffectId.Delegate {
  type Result[A]
  type ElimEffect
  type Signature[U] <: AnyRef
}


object Interpreter {
  // type Apply[O[_], L] = Interpreter {
  //   type Result[A] = O[A]
  //   type ElimEffect = L
  // }
}


object InterpreterCases {
  trait Unsealed extends Interpreter

  //@#@ moar
}
