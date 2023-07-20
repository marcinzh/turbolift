package turbolift.internals.primitives
import turbolift.{!!, Signature}
import turbolift.Computation.{Unsealed, Untyped}
import turbolift.HandlerCases.Primitive
import turbolift.internals.interpreter.Control
import turbolift.internals.engine.{Config, Prompt}


private[turbolift] object ComputationCases:
  final class Pure[A](val value: A) extends Unsealed[A, Any](Tags.Pure)
  final class Map[A, U](_tag: Byte, val comp: Untyped, val fun: Any => Any) extends Unsealed[A, U](_tag)
  final class ZipWithPar[A, B, C, U](val lhs: A !! U, val rhs: B !! U, val fun: (A, B) => C) extends Unsealed[C, U](Tags.ZipWithPar)
  final class Perform[A, U, Z <: Signature](val sig: Signature, val op: Z => Any) extends Unsealed[A, U](Tags.Perform)
  final class Impure[A, U](val thunk: () => A) extends Unsealed[A, U](Tags.Impure)
  final class Resume[A, U](val ctrl: Control.FlowUntyped, val value: Any, val stan: Any) extends Unsealed[A, U](Tags.Resume)
  final class Local[A, U](val ctrl: Control.FlowUntyped, val body: Untyped, val stan: Any) extends Unsealed[A, U](Tags.Local)
  final class Escape[A, U](val ctrl: Control.FlowUntyped | Null, val body: Untyped, val stan: Any) extends Unsealed[A, U](Tags.Escape)
  final class Handle[A, U, F[+_], G[+_], L, N](val body: F[A] !! (U & L), val handler: Primitive[F, G, L, N]) extends Unsealed[G[A], U & N](Tags.Handle)
  final class ConfigAsk[A](val fun: Config => A) extends Unsealed[A, Any](Tags.ConfigAsk)
  final class ConfigMod[A, U](val fun: Config => Config, val body: A !! U) extends Unsealed[A, U](Tags.ConfigMod)
