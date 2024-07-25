package turbolift.internals.primitives
import java.util.concurrent.TimeUnit
import turbolift.{!!, Signature}
import turbolift.Computation.{Unsealed, Untyped}
import turbolift.HandlerCases.Primitive
import turbolift.effects.IO
import turbolift.io.{Snap, Fiber, Warp, OnceVar, Zipper}
import turbolift.interpreter.{Interpreter, Continuation}
import turbolift.internals.engine.{Env}
import turbolift.internals.executor.Executor


private[turbolift] object ComputationCases:
  final class Pure[A](val value: A) extends Unsealed[A, Any](Tags.Pure)
  final class Map[A, U](_tag: Byte, val comp: Untyped, val fun: Any => Any) extends Unsealed[A, U](_tag)
  final class Perform[A, U, Z <: Signature](val sig: Signature, val op: Z => Any) extends Unsealed[A, U](Tags.Perform)
  final class ZipPar[A, B, C, U](val lhs: A !! U, val rhs: B !! U, val fun: (A, B) => C) extends Unsealed[C, U](Tags.ZipPar)
  final class OrPar[A, U](val lhs: A !! U, val rhs: A !! U) extends Unsealed[A, U](Tags.OrPar)
  final class OrSeq[A, U](val lhs: A !! U, val rhsFun: () => A !! U) extends Unsealed[A, U](Tags.OrSeq)
  final class Impure[A, U](val thunk: () => A) extends Unsealed[A, U](Tags.Impure)
  final class LocalGet(val prompt: Interpreter.Untyped) extends Unsealed[Any, Any](Tags.LocalGet)
  final class LocalPut[S](val prompt: Interpreter.Untyped, val local: S) extends Unsealed[Unit, Any](Tags.LocalPut)
  final class LocalUpdate[A, S](val prompt: Interpreter.Untyped, val fun: S => (A, S)) extends Unsealed[A, Any](Tags.LocalUpdate)
  final class Resume[A, B, S, U](val cont: Continuation[A, B, S, U], val value: A, val local: S) extends Unsealed[B, U](Tags.Resume)
  final class Delimit[A, S, U](val prompt: Interpreter.Untyped, val body: Untyped, val local: S, val fun: (S => S) | Null) extends Unsealed[A, U](Tags.Delimit)
  final class Capture[A, B, S, U](val prompt: Interpreter.Untyped, val fun: ContFun[A, B, S, U]) extends Unsealed[A, U](Tags.Capture)
  final class Abort[A, U](val prompt: Interpreter.Untyped, val value: Any) extends Unsealed[A, U](Tags.Abort)
  final class Handle[A, U, F[+_], G[+_], L, N](val body: F[A] !! (U & L), val handler: Primitive[F, G, L, N]) extends Unsealed[G[A], U & N](Tags.Handle)
  final class DoIO[A, B](val thunk: () => A, val isAttempt: Boolean) extends Unsealed[B, IO](Tags.DoIO)
  final class DoSnap[A, U](val body: A !! U) extends Unsealed[Snap[A], U](Tags.DoSnap)
  final class Unsnap[A, U](val snap: Snap[A]) extends Unsealed[A, U](Tags.Unsnap)
  final class EnvAsk[A](val fun: Env => A) extends Unsealed[A, Any](Tags.EnvAsk)
  final class EnvMod[A, U](val fun: Env => Env, val body: A !! U) extends Unsealed[A, U](Tags.EnvMod)
  final class ForkFiber[A, U](val warp: Warp | Null, val comp: A !! U, val name: String, val callback: (Zipper.Untyped => Unit) | Null = null) extends Unsealed[Fiber[A, U], Any](Tags.ForkFiber)
  final class AwaitFiber[A, U](val fiber: Fiber.Untyped, val isCancel: Boolean, val isVoid: Boolean) extends Unsealed[A, U](Tags.AwaitFiber)
  object CurrentFiber extends Unsealed[Fiber.Untyped, Any](Tags.CurrentFiber)
  final class SpawnWarp[A, U](val exitMode: Warp.ExitMode, val body: A !! (U & Warp), val name: String) extends Unsealed[A, U](Tags.SpawnWarp)
  final class AwaitWarp(val warp: Warp, val isCancel: Boolean) extends Unsealed[Unit, IO](Tags.AwaitWarp)
  final class AwaitOnceVar[A](val ovar: OnceVar.Get[A]) extends Unsealed[A, IO](Tags.AwaitOnceVar)
  final class Blocking[A, B](val thunk: () => A, isAttempt: Boolean) extends Unsealed[B, IO](Tags.Blocking)
  final class Sleep(val length: Long, val unit: TimeUnit = TimeUnit.MILLISECONDS) extends Unsealed[Unit, IO](Tags.Sleep)
  final class Suppress[A, U](val body: A !! U, val delta: Int) extends Unsealed[A, U](Tags.Suppress)
  final class ExecOn[A, U](val exec: Executor, val body: A !! U) extends Unsealed[A, U](Tags.ExecOn)
  object Yield extends Unsealed[Unit, Any](Tags.Yield)


  type ContFun[A, B, S, U] = ContFun1[A, B, S, U] | ContFun2[A, B, S, U]
  type ContFun1[A, B, S, U] = Continuation[A, B, S, U] => B !! U
  type ContFun2[A, B, S, U] = (Continuation[A, B, S, U], S) => B !! U
