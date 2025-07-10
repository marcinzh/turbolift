package turbolift
import scala.annotation.experimental
import turbolift.{ComputationCases => CC}
import turbolift.internals.macros.Boilerplate
import turbolift.interpreter.{Interpreter => IC, Mixins}

/**
 * Base trait for any user-defined effect.
 *
 * Instances of `Effect` are used:
 * - To establish unique identity of the effect, both in type and value space.
 * - By effect users: to invoke operations of the effect.
 * - By creators of effect handlers: to access base classes needed for implementing interpreters for this effect.
 * 
 * Example:
 * {{{
 * import turbolift.{!!, Signature, Effect}}
 *
 * // Signature:
 * trait GoogleSignature extends Signature:
 *   def countPicturesOf(topic: String): Int !! ThisEffect
 *
 * // Boilerplate:
 * trait Google extends Effect[GoogleSignature] with GoogleSignature:
 *   final override def countPicturesOf(topic: String) = perform(_.countPicturesOf(topic))
 *
 * // Instantiaton establishes the identity:
 * case object MyGoogle extends Google
 * type MyGoogle = MyGoogle.type // optional
 *
 * // Invoking operations:
 * val program: Int !! MyGoogle = MyGoogle.countPicturesOf("cat")
 * }}}
 * 
 * For details, see [Defining your own effects and handlers](https://marcinzh.github.io/turbolift/custom/index.html).
 * 
 * @tparam Z The [[Signature]] of this effect.
 */

trait Effect[Z <: Signature]:
  self: Z =>
  final override type ThisEffect = this.type

  /** API for defining custom **effects**. */
  type ThisSignature[U] = Z & Signature { type ThisEffect = U }

  /** API for defining custom **effects**.
   *
   * Embeds an invocation of this [[Signature]]'s method into the [[Computation]] monad.
   * This provides mechanism for separation between syntax an semantics of effects.
   */
  final inline def perform[A, U <: ThisEffect](inline f: ThisSignature[U] => A !! U): A !! U =
    CC.perform[A, U, ThisSignature[U]](this, f)

  /** API for defining custom **effects**. */
  final def performNoInline[A, U <: ThisEffect](f: ThisSignature[U] => A !! U): A !! U =
    CC.perform[A, U, ThisSignature[U]](this, f)


  /** API for defining custom **handlers**.
   *
   * Helper object providing type definitions to be used for implementing [[turbolift.interpreter.Interpreter Interpreters]] for this effect.
   */
  val impl: Effect.Impl[this.type] = new Effect.Impl(Array(this))
  export impl.ThisHandler

  /** API for defining custom **handlers**.
   *
   * Combines with another [[Effect]] instance, for the purpose of sharing an [[interpreter.Interpreter Interpreter]].
   *
   * Example of application: implement `RWS`-alike handler for `Reader &! Writer &! State` composition of effects.
   */
  final def &![Fx2 <: Signature](fx2: Fx2) = new Effect.Combine2[this.type, fx2.type](this, fx2)


object Effect:
  /** API for defining custom **effects**.
   *
   * Macro generating "the boilerplate" part.
   * For each abstract method defined in the signature
   * a `final override` method is generated,
   * with body consisting of [[perform]] call.
   *
   * Usage:
   * {{{
   * val MyGoogle = Effect.boilerplate[MyGoogleSignature]
   * type MyGoogle = MyGoogle.type // optional
   * }}}
   */
  @experimental inline def boilerplate[Z <: Signature]: Effect[Z] & Z = ${ Boilerplate.macroImpl[Z] }


  /** API for defining custom **handlers**.
   *
   * Helper class providing type definitions to be used for implementing [[turbolift.interpreter.Interpreter Interpreters]] for this effect.
   * An instance of [Impl]] is available as [[Effect.impl]] value.
   *
   * @tparam Fx The effect we are defining interpreter/handler for.
   */
  final class Impl[Fx](private[turbolift] sigs: Array[Signature]):
    /** Alias for [[Handler]], specialized to eliminate this effect. */
    final type ThisHandler[F[+_], G[+_], N] = Handler[F, G, Fx, N]

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
  end Impl


  /** API for defining custom **handlers**.
   *
   * Composition of 2 effects, for the purpose of sharing an [[interpreter.Interpreter Interpreter]].
   */
  final class Combine2[Fx1 <: Signature, Fx2 <: Signature](val fx1: Fx1, val fx2: Fx2):
    val impl: Impl[fx1.type & fx2.type] = new Impl(Array(fx1, fx2))
    export impl.ThisHandler

    /** Combines with another [[Effect]] instance, for the purpose of sharing an [[interpreter.Interpreter Interpreter]]. */
    def &![Fx3 <: Signature](fx3: Fx3) = new Combine3[Fx1, Fx2, Fx3](fx1, fx2, fx3)


  /** API for defining custom **handlers**.
   *
   * Composition of 3 effects, for the purpose of sharing an [[interpreter.Interpreter Interpreter]].
   */
  final class Combine3[Fx1 <: Signature, Fx2 <: Signature, Fx3 <: Signature](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3):
    val impl: Impl[fx1.type & fx2.type & fx3.type] = new Impl(Array(fx1, fx2, fx3))
    export impl.ThisHandler
    
    /** Combines with another [[Effect]] instance, for the purpose of sharing an [[interpreter.Interpreter Interpreter]]. */
    def &![Fx4 <: Signature](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  /** API for defining custom **handlers**.
   *
   * Composition of 4 effects, for the purpose of sharing an [[interpreter.Interpreter Interpreter]].
   */
  final class Combine4[Fx1 <: Signature, Fx2 <: Signature, Fx3 <: Signature, Fx4 <: Signature](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4):
    val impl: Impl[fx1.type & fx2.type & fx3.type & fx4.type] = new Impl(Array(fx1, fx2, fx3, fx4))
    export impl.ThisHandler
