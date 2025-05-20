package turbolift
import scala.annotation.experimental
import turbolift.internals.effect.{EffectImpl, CanPerform, Boilerplate}
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
 * type MyGoogle = MyGoogle.type
 *
 * // Invoking operations:
 * val program: Int !! MyGoogle = MyGoogle.countPicturesOf("cat")
 * }}}
 * 
 * For details, see [Defining your own effects and handlers](https://marcinzh.github.io/turbolift/custom/index.html).
 * 
 * @tparam Z The [[Signature]] of this effect.
 */

trait Effect[Z <: Signature] extends CanPerform[Z]:
  self: Z =>
  final override type ThisEffect = this.type

  /** Object containing type definitions, to be used for implementing [[turbolift.interpreter.Interpreter Interpreters]] for this effect. */
  val impl: EffectImpl[this.type] = new EffectImpl(Array(this))
  export impl.ThisHandler


  /** Combines with another [[Effect]] instance, for the purpose of sharing an [[interpreter.Interpreter Interpreter]]. */
  final def &![Fx2 <: Signature](fx2: Fx2) = new Effect.Combine2[this.type, fx2.type](this, fx2)


object Effect:
  @experimental inline def boilerplate[Z <: Signature]: Effect[Z] & Z = ${ Boilerplate.macroImpl[Z] }


  final class Combine2[Fx1 <: Signature, Fx2 <: Signature](val fx1: Fx1, val fx2: Fx2):
    val impl: EffectImpl[fx1.type & fx2.type] = new EffectImpl(Array(fx1, fx2))
    export impl.ThisHandler

    /** Combines with another [[Effect]] instance, for the purpose of sharing an [[interpreter.Interpreter Interpreter]]. */
    def &![Fx3 <: Signature](fx3: Fx3) = new Combine3[Fx1, Fx2, Fx3](fx1, fx2, fx3)


  /** Composition of 3 effects, for the purpose of sharing an [[interpreter.Interpreter Interpreter]]. */
  final class Combine3[Fx1 <: Signature, Fx2 <: Signature, Fx3 <: Signature](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3):
    val impl: EffectImpl[fx1.type & fx2.type & fx3.type] = new EffectImpl(Array(fx1, fx2, fx3))
    export impl.ThisHandler
    
    /** Combines with another [[Effect]] instance, for the purpose of sharing an [[interpreter.Interpreter Interpreter]]. */
    def &![Fx4 <: Signature](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  /** Composition of 4 effects, for the purpose of sharing an [[interpreter.Interpreter Interpreter]]. */
  final class Combine4[Fx1 <: Signature, Fx2 <: Signature, Fx3 <: Signature, Fx4 <: Signature](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4):
    val impl: EffectImpl[fx1.type & fx2.type & fx3.type & fx4.type] = new EffectImpl(Array(fx1, fx2, fx3, fx4))
    export impl.ThisHandler
