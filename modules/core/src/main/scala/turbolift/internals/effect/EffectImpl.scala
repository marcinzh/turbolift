package turbolift.internals.effect
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.interpreter.{Interpreter => IC, Mixins}


final class EffectImpl[Fx](sigs: Array[Signature]):
  /** Alias for [[Handler]], specialized to eliminate this effect. */
  final type ThisHandler[F[+_], G[+_], N] = Handler[F, G, Fx, N]

  /** Namespace for convenient versions of `ThisHandler` type alias,
   *  specialized by partially applying some or all of its type parameters.
   *
   *  For example: `ThisHandler.FromId.ToId.Free`
   *  is equivalent of `ThisHandler[[X] =>> X, [X] =>> X, Any]`
   *
   *  This works like type-level "fluent interface", where:
   *  - `FromId` and `FromConst` partially apply `F[+_]` parameter of [[ThisHandler]] type.
   *  - `ToId` and `ToConst` partially apply `G[+_]` parameter of [[ThisHandler]] type.
   *  - `Free` partially applies `N` parameter of [[ThisHandler]] type.
   */
  object ThisHandler:
    type Free[F[+_], G[+_]] = Handler.Free[F, G, Fx]

    type FromId[F[+_], N] = Handler.FromId[F[+_], Fx, N]
    object FromId:
      type Free[F[+_]] = Handler.FromId.Free[F, Fx]

      type ToId[N] = Handler.FromId.ToId[Fx, N]
      object ToId:
        type Free = Handler.FromId.ToId.Free[Fx]

      type ToConst[D, N] = Handler.FromId.ToConst[D, Fx, N]
      object ToConst:
        type Free[D] = Handler.FromId.ToConst.Free[D, Fx]

    type FromConst[C, F[+_], N] = Handler.FromConst[C, F, Fx, N]
    object FromConst:
      type Free[C, F[+_]] = Handler.FromConst.Free[C, F, Fx]

      type ToConst[C, D, N] = Handler.FromConst.ToConst[C, D, Fx, N]
      object ToConst:
        type Free[C, D] = Handler.FromConst.ToConst.Free[C, D, Fx]

    type ToId[F[+_], N] = Handler.ToId[F[+_], Fx, N]
    object ToId:
      type Free[F[+_]] = Handler.ToId.Free[F[+_], Fx]

    type ToConst[F[+_], D, N] = Handler.ToConst[F[+_], D, Fx, N]
    object ToConst:
      type Free[F[+_], D] = Handler.ToConst.Free[F[+_], D, Fx]


  // private[turbolift] def signatures: Array[Signature]
  sealed trait ThisInterpreter extends IC.Unsealed:
    final override type ThisEffect = Fx
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


  /** Namespace for convenient versions of `Stateless` class,
   *  specialized by partially applying some or all of its type parameters.
   *
   *  For example: `Stateless.FromId.ToId.Free`
   *  is equivalent of `Stateless[[X] =>> X, [X] =>> X, ThisEffect, Any]`
   *
   *  This works like type-level "fluent interface", where:
   *  - `FromId` and `FromConst` partially apply `F[+_]` parameter of [[Stateless]] class.
   *  - `ToId` and `ToConst` partially apply `G[+_]` parameter of [[Stateless]] class.
   *  - `Free` partially applies `Fx` parameter of [[Stateless]] class.
   */
  object Stateless:
    abstract class FromId[G[+_], Fx] extends IC.Stateless[[X] =>> X, G, Fx] with ThisInterpreter
    object FromId:
      abstract class ToId[Fx] extends IC.Stateless[[X] =>> X, [X] =>> X, Fx] with ThisInterpreter
      object ToId:
        abstract class Free extends IC.Stateless[[X] =>> X, [X] =>> X, Any] with ThisInterpreter

      abstract class ToConst[D, Fx] extends IC.Stateless[[X] =>> X, [_] =>> D, Fx] with ThisInterpreter
      object ToConst:
        abstract class Free[D] extends IC.Stateless[[X] =>> X, [_] =>> D, Any] with ThisInterpreter

      abstract class Free[G[+_]] extends IC.Stateless[[X] =>> X, G, Any] with ThisInterpreter

    abstract class FromConst[C, G[+_], Fx] extends IC.Stateless[[_] =>> C, G, Fx] with ThisInterpreter
    object FromConst:
      abstract class ToConst[C, D, Fx] extends IC.Stateless[[_] =>> C, [_] =>> D, Fx] with ThisInterpreter

      object ToConst:
        abstract class Free[C, D] extends IC.Stateless[[_] =>> C, [_] =>> D, Any] with ThisInterpreter

      abstract class Free[C, G[+_]] extends IC.Stateless[[_] =>> C, G, Any] with ThisInterpreter


  /** Namespace for convenient versions of `Stateful` class,
   *  specialized by partially applying some or all of its type parameters.
   *
   *  For example: `Stateful.FromId.ToId.Free`
   *  is equivalent of `Stateful[[X] =>> X, [X] =>> X, ThisEffect, Any]`
   *
   *  This works like type-level "fluent interface", where:
   *  - `FromId` and `FromConst` partially apply `F[+_]` parameter of [[Stateful]] class.
   *  - `ToId` and `ToConst` partially apply `G[+_]` parameter of [[Stateful]] class.
   *  - `Free` partially applies `Fx` parameter of [[Stateful]] class.
   */
  object Stateful:
    abstract class FromId[G[+_], Fx] extends IC.Stateful[[X] =>> X, G, Fx] with ThisInterpreter
    object FromId:
      abstract class ToId[Fx] extends IC.Stateful[[X] =>> X, [X] =>> X, Fx] with ThisInterpreter
      object ToId:
        abstract class Free extends IC.Stateful[[X] =>> X, [X] =>> X, Any] with ThisInterpreter

      abstract class ToConst[D, Fx] extends IC.Stateful[[X] =>> X, [_] =>> D, Fx] with ThisInterpreter
      object ToConst:
        abstract class Free[D] extends IC.Stateful[[X] =>> X, [_] =>> D, Any] with ThisInterpreter

      abstract class Free[G[+_]] extends IC.Stateful[[X] =>> X, G, Any] with ThisInterpreter

    abstract class FromConst[C, G[+_], Fx] extends IC.Stateful[[_] =>> C, G, Fx] with ThisInterpreter
    object FromConst:
      abstract class ToConst[C, D, Fx] extends IC.Stateful[[_] =>> C, [_] =>> D, Fx] with ThisInterpreter

      object ToConst:
        abstract class Free[C, D] extends IC.Stateful[[_] =>> C, [_] =>> D, Any] with ThisInterpreter

      abstract class Free[C, G[+_]] extends IC.Stateful[[_] =>> C, G, Any] with ThisInterpreter


  export Mixins.{Sequential, Parallel}
