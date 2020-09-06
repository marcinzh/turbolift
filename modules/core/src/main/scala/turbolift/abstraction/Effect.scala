package turbolift.abstraction
import cats.Functor
import turbolift.abstraction.internals.effect.{HasEffectId, Embedding}
import turbolift.abstraction.internals.interpreter.{InterpreterCases => IC}
import turbolift.abstraction.internals.interpreter.{MonadTransformerCases => TC}


trait Effect[Z[_] <: AnyRef] extends Embedding[Z] with HasEffectId.Self {
  final override type ThisEffect = this.type
  final type ThisHandler[F[_]] = Handler[F, this.type]

  sealed trait ThisInterpreter extends IC.Unsealed {
    final override def effectIdDelegate = Effect.this
    final override type ElimEffect = ThisEffect
    final override type Signature[U] = Z[U]
  }

  abstract class Nullary[O[_]: Functor] extends TC.Nullary[O] with ThisInterpreter {
    final def self: Nullary[O] = this
    final override val theFunctor = Functor[O]
  }

  abstract class Unary[S, O[_]: Functor] extends TC.Unary[S, O] with ThisInterpreter {
    final def self: Unary[S, O] = this
    final override val theFunctor = Functor[O]
  }
}