package turbolift
import cats.Functor
import turbolift.internals.effect.{EffectId, HasEffectId, ProtoEffect}
import turbolift.internals.interpreter.{InterpreterCases => IC}


trait Effect[Z <: Signature] extends ProtoEffect[Z] with Effect.Stub:
  enclosing =>
  final override type ThisEffect = this.type
  final override type ThisSignature = Z

  final type ThisHandler[F[+_], N] = Handler[F, this.type, N]
  object ThisHandler:
    type Id[N] = Handler.Id[enclosing.type, N]
    type Free[F[+_]] = Handler.Free[F[+_], enclosing.type]
    type FreeId = Handler.FreeId[enclosing.type]

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


  abstract class Combine[Fx](val ids: EffectId*):
    sealed trait ThisInterpreter extends IC.Unsealed:
      final override val effectIds: Array[EffectId] = ids.toArray
      final override type ThisEffect = Fx

    final type ThisHandler[F[+_], N] = Handler[F, Fx, N]
    object ThisHandler:
      type Id[N] = Handler.Id[Fx, N]
      type Free[F[+_]] = Handler.Free[F[+_], Fx]
      type FreeId = Handler.FreeId[Fx]


  final class Combine2[Fx1 <: Stub, Fx2 <: Stub](val fx1: Fx1, val fx2: Fx2) extends Combine[Fx1 & Fx2](fx1, fx2):
    private type Z = fx1.ThisSignature & fx2.ThisSignature
    abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: Z => }
    abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: Z => }
    abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: Z => }
    def &![Fx3 <: Stub](fx3: Fx3) = new Combine3[fx1.type, fx2.type, fx3.type](fx1, fx2, fx3)


  final class Combine3[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3) extends Combine[Fx1 & Fx2 & Fx3](fx1, fx2, fx3):
    private type Z = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature
    abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: Z => }
    abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: Z => }
    abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: Z => }
    def &![Fx4 <: Stub](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  final class Combine4[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub, Fx4 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4) extends Combine[Fx1 & Fx2 & Fx3 & Fx4](fx1, fx2, fx3, fx4):
    private type Z = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature & fx4.ThisSignature
    abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: Z => }
    abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: Z => }
    abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: Z => }
