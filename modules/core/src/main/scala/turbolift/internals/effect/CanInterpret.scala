package turbolift.internals.effect
import turbolift.{Signature, Effect, Handler}
import turbolift.internals.interpreter.{Interpreter => IC, Features}


trait CanInterpret extends HasSignature:
  enclosing =>
  private[turbolift] def signatures: Array[Signature]

  /** Alias for [[Handler]], specialized to eliminate this effect. */
  final type ThisHandler[F[+_], G[+_], N] = Handler[F, G, ThisEffect, N]

  /** Defines type aliases for [[Handler]], specialized to eliminate this effect. */
  object ThisHandler:
    /** Alias for [[Handler.FromId]], specialized to eliminate this effect. */
    type FromId[F[+_], N] = Handler.FromId[F, ThisEffect, N]

    /** Alias for [[Handler.Id]], specialized to eliminate this effect. */
    type Id[N] = Handler.Id[ThisEffect, N]

    /** Alias for [[Handler.Const]], specialized to eliminate this effect. */
    type Const[C, N] = Handler.Const[C, ThisEffect, N]

    /** Alias for [[Handler.Free]], specialized to eliminate this effect. */
    type Free[F[+_]] = Handler.Free[F, ThisEffect]

    object Free:
      /** Alias for [[Handler.Free.Id]], specialized to eliminate this effect. */
      type Id = Handler.Free.Id[ThisEffect]

      /** Alias for [[Handler.Free.Const]], specialized to eliminate this effect. */
      type Const[C] = Handler.Free.Const[C, ThisEffect]


  sealed trait ThisInterpreter extends IC.Unsealed:
    final override type ThisEffect = enclosing.ThisEffect
    private[turbolift] final override val signatures: Array[Signature] = enclosing.signatures


  /** Base class for any user-defined stateless interpreter for this effect.
   *
   *  Like [[turbolift.internals.interpreter.Interpreter.Stateless Stateless Interpreter]], but specialized for this effect.
   */
  abstract class Stateless[F[+_], Fx] extends IC.Stateless[[X] =>> X, F, Fx] with ThisInterpreter { thiz: ThisSignature => }

  /** Base class for any user-defined stateful interpreter for this effect.
   *
   *  Like [[turbolift.internals.interpreter.Interpreter.Stateful Stateful Interpreter]], but specialized for this effect.
   */
  abstract class Stateful[S, F[+_], Fx] extends IC.Stateful[S, [X] =>> X, F, Fx] with ThisInterpreter { thiz: ThisSignature => }


  /** Namespace for abstract interpreters, that are "Const".
   *
   *  Such interpreters produce handlers, that are applicable only to computations,
   *  that return values of some specific type. This type supplied in constructor parameter.
   *  This is opposed to the default case, where produced handler is universally quantified over that type.
   */
  object Const:
    /** Like [[Stateless]], but obtained handler is applicable only to computations that return `C`. */
    abstract class Stateless[C, F[+_], Fx] extends IC.Stateless[[_] =>> C, F, Fx] with ThisInterpreter { thiz: ThisSignature => }

    /** Like [[Stateful]], but obtained handler is applicable only to computations that return `C`. */
    abstract class Stateful[C, S, F[+_], Fx] extends IC.Stateful[S, [_] =>> C, F, Fx] with ThisInterpreter { thiz: ThisSignature => }


  /** Namespace for abstract interpreters, that are "Free", meaning they have no dependencies. */
  object Free:
    /** Like [[Stateless]], except this one has no dependencies. */
    abstract class Stateless[F[+_]] extends IC.Stateless[[X] =>> X, F, Any] with ThisInterpreter { thiz: ThisSignature => }

    /** Like [[Stateful]], except this one has no dependencies. */
    abstract class Stateful[S, F[+_]] extends IC.Stateful[S, [X] =>> X, F, Any] with ThisInterpreter { thiz: ThisSignature => }

    /** Namespace for abstract interpreters, that are both "Const" and "Free". */
    object Const:
      /** Like [[Stateless]], but obtained handler is applicable only to computations that return `C`. */
      abstract class Stateless[C, F[+_]] extends IC.Stateless[[_] =>> C, F, Any] with ThisInterpreter { thiz: ThisSignature => }

      /** Like [[Stateful]], but obtained handler is applicable only to computations that return `C`. */
      abstract class Stateful[C, S, F[+_]] extends IC.Stateful[S, [_] =>> C, F, Any] with ThisInterpreter { thiz: ThisSignature => }


  /** Base class for any user-defined proxy interpreter for this effect.
   *
   *  Like [[turbolift.internals.interpreter.Interpreter.Proxy Proxy Interpreter]], but specialized for this effect.
   */
  abstract class Proxy[Fx] extends IC.Proxy[Fx] with ThisInterpreter { thiz: ThisSignature => }

  abstract class ProxyIO extends IC.ProxyIO with ThisInterpreter { thiz: ThisSignature => }


  export Features.{Sequential, Parallel}
