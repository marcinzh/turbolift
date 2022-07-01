package turbolift
import cats.Functor
import turbolift.internals.effect.{EffectStub => Stub, ProtoEffect}
import turbolift.internals.interpreter.{InterpreterCases => IC}


trait Effect[Z <: Signature] extends ProtoEffect[Z]:
  enclosing =>
  final override type ThisEffect = this.type
  private[turbolift] final override type ThisSignature = Z

  final type ThisHandler[F[+_], N] = Handler[F, this.type, N]
  object ThisHandler:
    type Id[N] = Handler.Id[enclosing.type, N]
    type Const[T, N] = Handler.Const[T, enclosing.type, N]
    type Free[F[+_]] = Handler.Free[F[+_], enclosing.type]
    type FreeId = Handler.FreeId[enclosing.type]
    type FreeConst[T] = Handler.FreeConst[T, enclosing.type]

  sealed trait ThisInterpreter extends IC.Unsealed:
    final override val signatures: Array[Signature] = Array(Effect.this)
    final override type ThisEffect = enclosing.ThisEffect

  abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: Z => }
  abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: Z => }
  abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: Z => }

  final def &![Fx2 <: Stub](fx2: Fx2) = new Effect.Combine2[this.type, Fx2](this, fx2)


object Effect:
  private[turbolift] abstract class Combine(val sigs: Signature*):
    //// Surprisingly, it type checks better as type member, rather than type parameter
    protected type CombinedFx
    protected type CombinedSig

    sealed trait ThisInterpreter extends IC.Unsealed:
      final override val signatures: Array[Signature] = sigs.toArray
      final override type ThisEffect = CombinedFx

    final type ThisHandler[F[+_], N] = Handler[F, CombinedFx, N]
    object ThisHandler:
      type Id[N] = Handler.Id[CombinedFx, N]
      type Const[T, N] = Handler.Const[T, CombinedFx, N]
      type Free[F[+_]] = Handler.Free[F[+_], CombinedFx]
      type FreeId = Handler.FreeId[CombinedFx]
      type FreeConst[T] = Handler.FreeConst[T, CombinedFx]

    abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: CombinedSig => }
    abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: CombinedSig => }
    abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: CombinedSig => }


  final class Combine2[Fx1 <: Stub, Fx2 <: Stub](val fx1: Fx1, val fx2: Fx2) extends Combine(fx1, fx2):
    override protected type CombinedFx = fx1.type & fx2.type
    override protected type CombinedSig = fx1.ThisSignature & fx2.ThisSignature
    def &![Fx3 <: Stub](fx3: Fx3) = new Combine3[Fx1, Fx2, Fx3](fx1, fx2, fx3)


  final class Combine3[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3) extends Combine(fx1, fx2, fx3):
    override protected type CombinedFx = fx1.type & fx2.type & fx3.type
    override protected type CombinedSig = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature
    def &![Fx4 <: Stub](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  final class Combine4[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub, Fx4 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4) extends Combine(fx1, fx2, fx3, fx4):
    override protected type CombinedFx = fx1.type & fx2.type & fx3.type & fx4.type
    override protected type CombinedSig = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature & fx4.ThisSignature
