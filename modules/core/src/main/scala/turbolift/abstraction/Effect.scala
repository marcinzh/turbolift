package turbolift.abstraction
import cats.Functor
import turbolift.abstraction.internals.effect.{EffectId, HasEffectId, ProtoEffect}
import turbolift.abstraction.internals.interpreter.{InterpreterCases => IC}


trait Effect[Z <: Signature] extends ProtoEffect[Z] with Effect.Stub:
  enclosing =>
  final override type ThisEffect = this.type
  final override type ThisSignature = Z
  final type ThisHandler[F[+_], N] = Handler[F, this.type, N]
  final type ThisIHandler[F[+_]] = ThisHandler[F, Any]
  final type ThisIdHandler[N] = ThisHandler[[X] =>> X, N]
  final type ThisIIdHandler = ThisIdHandler[Any]

  sealed trait ThisInterpreter extends IC.Unsealed:
    final override val effectIds: Array[EffectId] = Array(Effect.this)
    final override type ThisEffect = enclosing.ThisEffect

  abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: Z => }
  abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: Z => }
  abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: Z => }

  final def &![Fx2 <: Effect.Stub](fx2: Fx2) = new Effect.Combine2[this.type, Fx2](this, fx2)


object Effect:
  sealed trait Stub extends HasEffectId.Self:
    type ThisSignature <: Signature

  final class Combine2[Fx1 <: Stub, Fx2 <: Stub](val fx1: Fx1, val fx2: Fx2):
    private type Fx = Fx1 with Fx2
    private type Z = fx1.ThisSignature with fx2.ThisSignature

    type ThisHandler[F[+_], N] = Handler[F, Fx, N]
    type ThisIHandler[F[+_]] = ThisHandler[F, Any]
    type ThisIdHandler[N] = ThisHandler[[X] =>> X, N]
    type ThisIIdHandler = ThisIdHandler[Any]

    sealed trait ThisInterpreter extends IC.Unsealed:
      final override val effectIds: Array[EffectId] = Array(fx1, fx2)
      final override type ThisEffect = Fx

    abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: Z => }
    abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: Z => }
    abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: Z => }

    def &![Fx3 <: Stub](fx3: Fx3) = new Combine3[Fx1, Fx2, Fx3](fx1, fx2, fx3)


  final class Combine3[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3):
    private type Fx = Fx1 with Fx2 with Fx3
    private type Z = fx1.ThisSignature with fx2.ThisSignature with fx3.ThisSignature

    type ThisHandler[F[+_], N] = Handler[F, Fx, N]
    type ThisIHandler[F[+_]] = ThisHandler[F, Any]
    type ThisIdHandler[N] = ThisHandler[[X] =>> X, N]
    type ThisIIdHandler = ThisIdHandler[Any]

    sealed trait ThisInterpreter extends IC.Unsealed:
      final override val effectIds: Array[EffectId] = Array(fx1, fx2, fx3)
      final override type ThisEffect = Fx

    abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: Z => }
    abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: Z => }
    abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: Z => }

    def &![Fx4 <: Stub](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  final class Combine4[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub, Fx4 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4):
    private type Fx = Fx1 with Fx2 with Fx3 with Fx4
    private type Z = fx1.ThisSignature with fx2.ThisSignature with fx3.ThisSignature with fx4.ThisSignature

    type ThisHandler[F[+_], N] = Handler[F, Fx, N]
    type ThisIHandler[F[+_]] = ThisHandler[F, Any]
    type ThisIdHandler[N] = ThisHandler[[X] =>> X, N]
    type ThisIIdHandler = ThisIdHandler[Any]

    sealed trait ThisInterpreter extends IC.Unsealed:
      final override val effectIds: Array[EffectId] = Array(fx1, fx2, fx3, fx4)
      final override type ThisEffect = Fx

    abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: Z => }
    abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: Z => }
    abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: Z => }
