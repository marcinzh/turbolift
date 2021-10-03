package turbolift.abstraction
import cats.Functor
import turbolift.abstraction.internals.effect.{EffectId, HasEffectId, ProtoEffect}
import turbolift.abstraction.internals.interpreter.{InterpreterCases => IC}
import turbolift.abstraction.internals.interpreter.{MonadTransformerCases => TC}


trait HasSig extends HasEffectId.Self:
  type ThisSig[_] <: AnyRef


trait Effect[Z[_] <: AnyRef] extends ProtoEffect[Z] with HasSig:
  final override type ThisEffect = this.type
  final override type ThisSig[U] = Z[U]
  final type ThisHandler[F[_], N] = Handler[F, this.type, N]
  final type ThisIHandler[F[_]] = IHandler[F, this.type]

  sealed trait ThisInterpreter extends IC.Unsealed:
    final override val effectIds: Vector[EffectId] = Vector(Effect.this)
    final override type ElimEffect = ThisEffect
    final override type Signature[U] = Z[U]

  abstract class Stateless[O[_]: Functor] extends TC.Stateless[O] with ThisInterpreter
  abstract class Stateful[S, O[_]: Functor] extends TC.Stateful[S, O] with ThisInterpreter
  abstract class Proxy[Fx] extends IC.Proxy[Fx] with ThisInterpreter

  final def &![Fx2 <: HasSig](fx2: Fx2) = new Effect.Combine2[this.type, Fx2](this, fx2)


object Effect:
  class Combine2[Fx1 <: HasSig, Fx2 <: HasSig](val fx1: Fx1, val fx2: Fx2):
    final type ThisHandler[F[_], N] = Handler[F, Fx1 with Fx2, N]
    final type ThisIHandler[F[_]] = IHandler[F, Fx1 with Fx2]

    sealed trait ThisInterpreter extends IC.Unsealed:
      final override val effectIds: Vector[EffectId] = Vector(fx1, fx2)
      final override type ElimEffect = Fx1 with Fx2
      final override type Signature[U] = fx1.ThisSig[U] with fx2.ThisSig[U]

    abstract class Stateless[O[_]: Functor] extends TC.Stateless[O] with ThisInterpreter
    abstract class Stateful[S, O[_]: Functor] extends TC.Stateful[S, O] with ThisInterpreter
    abstract class Proxy[Fx] extends IC.Proxy[Fx] with ThisInterpreter

    final def &![Fx3 <: HasSig](fx3: Fx3) = new Combine3[Fx1, Fx2, Fx3](fx1, fx2, fx3)


  class Combine3[Fx1 <: HasSig, Fx2 <: HasSig, Fx3 <: HasSig](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3):
    final type ThisHandler[F[_], N] = Handler[F, Fx1 with Fx2 with Fx3, N]
    final type ThisIHandler[F[_]] = IHandler[F, Fx1 with Fx2 with Fx3]

    sealed trait ThisInterpreter extends IC.Unsealed:
      final override val effectIds: Vector[EffectId] = Vector(fx1, fx2, fx3)
      final override type ElimEffect = Fx1 with Fx2 with Fx3
      final override type Signature[U] = fx1.ThisSig[U] with fx2.ThisSig[U] with fx3.ThisSig[U]

    abstract class Stateless[O[_]: Functor] extends TC.Stateless[O] with ThisInterpreter
    abstract class Stateful[S, O[_]: Functor] extends TC.Stateful[S, O] with ThisInterpreter
    abstract class Proxy[Fx] extends IC.Proxy[Fx] with ThisInterpreter

    final def &![Fx4 <: HasSig](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  class Combine4[Fx1 <: HasSig, Fx2 <: HasSig, Fx3 <: HasSig, Fx4 <: HasSig](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4):
    final type ThisHandler[F[_], N] = Handler[F, Fx1 with Fx2 with Fx3 with Fx4, N]
    final type ThisIHandler[F[_]] = IHandler[F, Fx1 with Fx2 with Fx3 with Fx4]

    sealed trait ThisInterpreter extends IC.Unsealed:
      final override val effectIds: Vector[EffectId] = Vector(fx1, fx2, fx3, fx4)
      final override type ElimEffect = Fx1 with Fx2 with Fx3 with Fx4
      final override type Signature[U] = fx1.ThisSig[U] with fx2.ThisSig[U] with fx3.ThisSig[U] with fx4.ThisSig[U]

    abstract class Stateless[O[_]: Functor] extends TC.Stateless[O] with ThisInterpreter
    abstract class Stateful[S, O[_]: Functor] extends TC.Stateful[S, O] with ThisInterpreter
    abstract class Proxy[Fx] extends IC.Proxy[Fx] with ThisInterpreter
