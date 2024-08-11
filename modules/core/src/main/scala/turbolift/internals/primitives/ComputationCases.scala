package turbolift.internals.primitives
import java.util.concurrent.TimeUnit
import turbolift.{!!, Signature}
import turbolift.Computation.{Unsealed, Untyped}
import turbolift.interpreter.Interpreter 
import turbolift.internals.engine.Engine
import turbolift.internals.executor.Executor


//@#@TEMP public bcoz inline bug
// private[turbolift] object ComputationCases:
object ComputationCases:
  abstract class Map[A, B, C, U](_tag: Int, val comp: A !! U) extends Unsealed[B, U](_tag) with Function1[A, C]
  abstract class Perform[A, U, Z <: Signature](val sig: Signature) extends Unsealed[A, U](Tags.Perform) with Function1[Z, A !! U]
  abstract class Impure[A]() extends Unsealed[A, Any](Tags.Impure) with Function0[A]
  abstract class LocalUpdate[A, S](val prompt: Interpreter.Untyped) extends Unsealed[A, Any](Tags.LocalUpdate) with Function1[S, (A, S)]

  sealed abstract class Intristic[A, U] extends Unsealed[A, U](Tags.Intristic) with Function[Engine, Engine.IntristicResult]

  inline def intristic[A, U](f: Engine => Engine.IntristicResult): A !! U =
    new Intristic[A, U]:
      override def apply(engine: Engine): Engine.IntristicResult = f(engine)

  final class Pure[A](val value: A) extends Unsealed[A, Any](Tags.Pure)
  final class LocalGet(val prompt: Interpreter.Untyped) extends Unsealed[Any, Any](Tags.LocalGet)
  final class LocalPut[S](val prompt: Interpreter.Untyped, val local: S) extends Unsealed[Unit, Any](Tags.LocalPut)
