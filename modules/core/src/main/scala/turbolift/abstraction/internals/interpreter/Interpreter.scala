package turbolift.abstraction.internals.interpreter
import cats.Id
import turbolift.abstraction.internals.effect.HasEffectId
import turbolift.abstraction.Handler


sealed trait Interpreter extends HasEffectId.Delegate {
  type Result[A]
  type ElimEffect
  type IntroEffect
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

  trait Independent extends Interpreter {
    final override type IntroEffect = Any
  }

  trait Dependent[TargetEffect] extends Interpreter {
    final override type Result[A] = A
    final override type IntroEffect = TargetEffect
    
    def interpret[U <: TargetEffect]: Signature[U]

    final def toHandler: Handler[Id, ElimEffect, IntroEffect] = ???
  }
}
