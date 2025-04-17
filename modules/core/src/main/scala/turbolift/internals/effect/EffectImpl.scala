package turbolift.internals.effect
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.interpreter.{Interpreter => IC, Mixins}


final class EffectImpl[Fx](sigs: Array[Signature]):
  /** Alias for [[Handler]], specialized to eliminate this effect. */
  final type ThisHandler[F[+_], G[+_], N] = Handler[F, G, Fx, N]

  // private[turbolift] def signatures: Array[Signature]
  sealed trait ThisInterpreter extends IC.Unsealed:
    final override type Elim = Fx
    private[turbolift] final override def enumSignatures: Array[Signature] = sigs


  /** Base class for any user-defined proxy interpreter for this effect.
   *
   *  Like [[turbolift.interpreter.Interpreter.Proxy Proxy Interpreter]], but specialized for this effect.
   */
  abstract class Proxy[Fx] extends IC.Proxy[Fx] with ThisInterpreter


  /** Base class for any user-defined stateless interpreter for this effect.
   *
   *  Like [[turbolift.interpreter.Interpreter.Stateless Stateless]] interpreter, but specialized for this effect.
   */
  abstract class Stateless[F[+_], G[+_], Fx] extends IC.Stateless[F, G, Fx] with ThisInterpreter


  /** Base class for any user-defined stateful interpreter for this effect.
   *
   *  Like [[turbolift.interpreter.Interpreter.Stateful Stateful]] interpreter, but specialized for this effect.
   */
  abstract class Stateful[F[+_], G[+_], Fx] extends IC.Stateful[F, G, Fx] with ThisInterpreter


  export Mixins.{Sequential, Parallel}
