package turbolift.abstraction
import cats.Functor
import turbolift.abstraction.internals.effect.{HasEffectId, ProtoEffect}
import turbolift.abstraction.internals.interpreter.{InterpreterCases => IC}
import turbolift.abstraction.internals.interpreter.{MonadTransformerCases => TC}


trait Effect[Z[_] <: AnyRef] extends ProtoEffect[Z] with HasEffectId.Self:
  final override type ThisEffect = this.type
  final type ThisHandler[F[_], N] = Handler[F, this.type, N]
  final type ThisIHandler[F[_]] = IHandler[F, this.type]

  sealed trait ThisInterpreter extends IC.Unsealed:
    final override def effectIdDelegate = Effect.this
    final override type ElimEffect = ThisEffect
    final override type Signature[U] = Z[U]

  abstract class Stateless[O[_]: Functor] extends TC.Stateless[O] with ThisInterpreter

  abstract class Stateful[S, O[_]: Functor] extends TC.Stateful[S, O] with ThisInterpreter

  abstract class Proxy[Fx] extends IC.Proxy[Fx] with ThisInterpreter
