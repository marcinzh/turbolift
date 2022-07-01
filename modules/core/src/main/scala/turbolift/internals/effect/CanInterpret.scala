package turbolift.internals.effect
import cats.Functor
import turbolift.{Signature, Effect, Handler}
import turbolift.internals.interpreter.{InterpreterCases => IC}


private[turbolift] trait CanInterpret extends HasSignature:
  enclosing =>
  def signatures: Array[Signature]

  final type ThisHandler[F[+_], N] = Handler[F, ThisEffect, N]
  object ThisHandler:
    type Id[N] = Handler.Id[ThisEffect, N]
    type Const[T, N] = Handler.Const[T, ThisEffect, N]
    type Free[F[+_]] = Handler.Free[F[+_], ThisEffect]
    type FreeId = Handler.FreeId[ThisEffect]
    type FreeConst[T] = Handler.FreeConst[T, ThisEffect]

  sealed trait ThisInterpreter extends IC.Unsealed:
    final override type ThisEffect = enclosing.ThisEffect
    final override val signatures: Array[Signature] = enclosing.signatures

  abstract class Stateless[F[+_]: Functor] extends IC.Stateless[F] with ThisInterpreter { thiz: ThisSignature => }
  abstract class Stateful[S, F[+_]: Functor] extends IC.Stateful[S, F] with ThisInterpreter { thiz: ThisSignature => }
  abstract class Proxy[Fx] extends IC.ProxyWithParam[Fx] with ThisInterpreter { thiz: ThisSignature => }
