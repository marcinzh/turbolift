package turbolift
import turbolift.internals.effect.{CanPerform, CanInterpret, HasSignature => Stub}

/**
 * Base trait for any user-defined effect.
 *
 * Instances of `Effect` are used for:
 * - Establishing unique identity of the effect.
 * - Invoking operations of the effect.
 * 
 * Example:
 * {{{
 * import turbolift.{!!, Signature, Effect}}
 *
 * // Signature:
 * trait GoogleSignature extends Signature:
 *   def countPicturesOf(topic: String): Int !@! ThisEffect
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

trait Effect[Z <: Signature] extends CanPerform[Z] with CanInterpret:
  enclosing =>
  final override type ThisEffect = this.type
  private[turbolift] final override type ThisSignature = Z
  private[turbolift] final override def signatures: Array[Signature] = Array(this)
  
  /** Combines with another [[Effect]] instance, for the purpose of sharing an [[internals.interpreter.Interpreter Interpreter]]. */
  final def &![Fx2 <: Stub](fx2: Fx2) = new Effect.Combine2[this.type, Fx2](this, fx2)


object Effect:
  private[turbolift] sealed abstract class Combine(sigs: Signature*) extends CanInterpret:
    private[turbolift] final override val signatures: Array[Signature] = sigs.toArray

  /** Composition of 2 effects, for the purpose of sharing an [[internals.interpreter.Interpreter Interpreter]]. */
  final class Combine2[Fx1 <: Stub, Fx2 <: Stub](val fx1: Fx1, val fx2: Fx2) extends Combine(fx1, fx2):
    override type ThisEffect = fx1.type & fx2.type
    private[turbolift] override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature
    
    /** Combines with another [[Effect]] instance, for the purpose of sharing an [[internals.interpreter.Interpreter Interpreter]]. */
    def &![Fx3 <: Stub](fx3: Fx3) = new Combine3[Fx1, Fx2, Fx3](fx1, fx2, fx3)


  /** Composition of 3 effects, for the purpose of sharing an [[internals.interpreter.Interpreter Interpreter]]. */
  final class Combine3[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3) extends Combine(fx1, fx2, fx3):
    override type ThisEffect = fx1.type & fx2.type & fx3.type
    private[turbolift] override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature
    
    /** Combines with another [[Effect]] instance, for the purpose of sharing an [[internals.interpreter.Interpreter Interpreter]]. */
    def &![Fx4 <: Stub](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  /** Composition of 4 effects, for the purpose of sharing an [[internals.interpreter.Interpreter Interpreter]]. */
  final class Combine4[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub, Fx4 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4) extends Combine(fx1, fx2, fx3, fx4):
    override type ThisEffect = fx1.type & fx2.type & fx3.type & fx4.type
    private[turbolift] override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature & fx4.ThisSignature
