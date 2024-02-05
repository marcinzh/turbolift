package turbolift.internals.primitives
import turbolift.{!!, Signature}
import turbolift.Computation.{Unsealed, Untyped}
import turbolift.HandlerCases.Primitive
import turbolift.io.Snap
import turbolift.interpreter.Control
import turbolift.internals.engine.{Env, Mark}


private[turbolift] object ComputationCases:
  final class Pure[A](val value: A) extends Unsealed[A, Any](Tags.Pure)
  final class Map[A, U](_tag: Byte, val comp: Untyped, val fun: Any => Any) extends Unsealed[A, U](_tag)
  final class Perform[A, U, Z <: Signature](val sig: Signature, val op: Z => Any) extends Unsealed[A, U](Tags.Perform)
  final class ZipPar[A, B, C, U](val lhs: A !! U, val rhs: B !! U, val fun: (A, B) => C) extends Unsealed[C, U](Tags.ZipPar)
  final class ZipSeq[A, B, C, U](val lhs: A !! U, val rhsFun: () => B !! U, val fun: (A, B) => C) extends Unsealed[C, U](Tags.ZipSeq)
  final class OrPar[A, U](val lhs: A !! U, val rhs: A !! U) extends Unsealed[A, U](Tags.OrPar)
  final class OrSeq[A, U](val lhs: A !! U, val rhsFun: () => A !! U) extends Unsealed[A, U](Tags.OrSeq)
  final class Impure[A, U](val thunk: () => A) extends Unsealed[A, U](Tags.Impure)
  final class Resume[A, S, U](val control: Control.Untyped, val value: Any, val stan: S) extends Unsealed[A, U](Tags.Resume)
  final class Local[A, S, U](val control: Control.Untyped, val body: Untyped, val stan: S) extends Unsealed[A, U](Tags.Local)
  final class Escape[A, S, U](val control: Control.Untyped, val body: Untyped, val stan: S) extends Unsealed[A, U](Tags.Escape)
  final class Abort[A, U](val control: Control.Untyped, val value: Any) extends Unsealed[A, U](Tags.Abort)
  final class Handle[A, U, F[+_], G[+_], L, N](val body: F[A] !! (U & L), val handler: Primitive[F, G, L, N]) extends Unsealed[G[A], U & N](Tags.Handle)
  final class EnvAsk[A](val fun: Env => A) extends Unsealed[A, Any](Tags.EnvAsk)
  final class EnvMod[A, U](val fun: Env => Env, val body: A !! U) extends Unsealed[A, U](Tags.EnvMod)
  final class DoSnap[A, B, U](val body: A !! U, val fun: Snap[A] => B !! U) extends Unsealed[B, U](Tags.DoSnap)
  final class Unsnap[A](val snap: Snap[A]) extends Unsealed[A, Any](Tags.Unsnap)
  object Yield extends Unsealed[Unit, Any](Tags.Yield)
  final class Try[A, U](val thunk: () => A) extends Unsealed[A, U](Tags.Try)
