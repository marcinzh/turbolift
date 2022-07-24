package turbolift
import turbolift.internals.effect.{CanPerform, CanInterpret, HasSignature => Stub}

/**
 *  Base trait for any user-defined effect.
 *
 *  An instance of [[Effect]] serves as a bridge, between effects request and effect handlers.
 *
 *  1. For effect requests, [[Effect]] provides syntax for invoking effect's operations. The syntax is defined using [[perform]] method. 
 *
 *  2. For effect handlers, [[Effect]] provides environment for implementing an [[internals.interpreter.Interpreter Interpreter]], which can be subsequently transformed to a [[Handler]].
 *
 *  Typically, a custom defined [[Signature]] is 1-1 paired with a custom defined [[Effect]].
 *
 *  --- 
 *
 *  # Usage
 *
 *  Assuming the following [[Signature]] defined:
 *  {{{
 *  import turbolift.Signature
 *
 *  trait GoogleSignature extends Signature:
 *    def countPicturesOf(topic: String): Int !@! ThisEffect
 *  }}}
 *
 *  ...then, the corresponding [[Effect]] should look like this:
 *  {{{
 *  import turbolift.{!!, Effect}
 *
 *  trait Google extends Effect[GoogleSignature] with GoogleSignature:
 *    final override def countPicturesOf(topic: String): Int !! this.type = perform(_.countPicturesOf(topic))
 *  }}}
 *
 *  ⚠️ `Google` uses its `GoogleSignature` **twice**: first as the type parameter, and second as the super trait.
 *
 *  ⚠️ [[Effect]] trait finally-overrides `!@!` as `!!`, and [[Signature.ThisEffect ThisEffect]] as `this.type`. 
 *  The (re)definintion of `countPicturesOf` in `Google` uses those overrides. This is not necessary, but it improves readability of error messages.
 *
 *  [[Effect]] instance defines unique identity for the effect, both in type and value spaces.
 *  In order for our `Google` to be usable, such instance must be made accessible. Assuming global scope:
 *  {{{
 *  case object MyGoogle extends Google   // unique value
 *  type MyGoogle = MyGoogle.type         // unique type (Scala's singleton type)
 *  }}}
 *  The type alias `type MyGoogle` is for convenience only.
 *   
 *  Now, we can finally invoke the effect's operations:
 *  {{{
 *  val myComputation: Int !! MyGoogle = MyGoogle.countPicturesOf("cat")
 *  }}}
 *   
 *  ⚠️ Unlike in most effect systems in Scala and Haskell scene, in Turbolift it is possible
 *  to have more than 1 instance of given effect, and even use them **simultaneously** in the same computation.
 *  Turbolift's runtime will treat them as completely separate effects, with each expecting a separate handler instance.
 *   
 *  **Related reading**: [[https://docs.idris-lang.org/en/latest/effects/state.html#labelled-effects Labelled Effects in Idris]]. In Idris, the label is optional.
 *  In Turbolift, effects are always "labelled". Scala's dependent typing can be used to write code polymorphic over effect's identity. Such as
 *  a [[Handler]], that can handle any instance of `Google` effect. This is how default handlers for standard effects (`Reader`, `State`, etc.) are implemented.
 *  
 *  Example of 2 instances of our `Google` effect:
 *  {{{
 *  case object MyGoogle1 extends Google
 *  case object MyGoogle2 extends Google
 *  type MyGoogle1 = MyGoogle1.type
 *  type MyGoogle2 = MyGoogle2.type
 *
 *  val myComputation: Int !! (MyGoogle1 & MyGoogle2) =
 *    for
 *      a <- MyGoogle1.countPicturesOf("cat")
 *      b <- MyGoogle2.countPicturesOf("dog")
 *    yield a + b
 *  }}}
 *   
 *  If, for some reasons, this property is undesirable, we can hardcode the effect to be limited to 1 instance forever.
 *  All we need to do, is to replace `trait Google` with `case object Google`:
 *  {{{
 *  trait Google extends Effect[GoogleSignature] with GoogleSignature:
 *    final override def countPicturesOf(topic: String): Int !! this.type = perform(_.countPicturesOf(topic))
 *  }}}
 *  ...with:
 *  {{{
 *  case object Google extends Effect[GoogleSignature] with GoogleSignature:
 *    final override def countPicturesOf(topic: String): Int !! Google = perform(_.countPicturesOf(topic))
 *
 *  type Google = Google.type
 *  }}}
 *   
 *  ⚠️ Even though we have just limited the number of `Google` effect instances to 1,
 *  we can still define multiple [[Handler Handlers]] for `Google`.
 *  
 *  @tparam Z The [[Signature]] of this effect.
 *   
 */

trait Effect[Z <: Signature] extends CanPerform[Z] with CanInterpret:
  enclosing =>
  final override type ThisEffect = this.type
  private[turbolift] final override type ThisSignature = Z
  private[turbolift] final override def signatures: Array[Signature] = Array(this)
  
  /** Combines with another [[Effect]] instance, so that a single [[Handler]] for 2 effects can be created. */
  final def &![Fx2 <: Stub](fx2: Fx2) = new Effect.Combine2[this.type, Fx2](this, fx2)


object Effect:
  private[turbolift] sealed abstract class Combine(sigs: Signature*) extends CanInterpret:
    private[turbolift] final override val signatures: Array[Signature] = sigs.toArray

  /** Provides environment for implementing [[Handler Handlers]], that handle 2 [[Effect Effects]] at once. */
  final class Combine2[Fx1 <: Stub, Fx2 <: Stub](val fx1: Fx1, val fx2: Fx2) extends Combine(fx1, fx2):
    override type ThisEffect = fx1.type & fx2.type
    private[turbolift] override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature
    
    /** Combines with another [[Effect]] instance, so that a single [[Handler]] for 3 effects can be created. */
    def &![Fx3 <: Stub](fx3: Fx3) = new Combine3[Fx1, Fx2, Fx3](fx1, fx2, fx3)


  /** Provides environment for implementing [[Handler Handlers]], that handle 3 [[Effect Effects]] at once. */
  final class Combine3[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3) extends Combine(fx1, fx2, fx3):
    override type ThisEffect = fx1.type & fx2.type & fx3.type
    private[turbolift] override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature
    
    /** Combines with another [[Effect]] instance, so that a single [[Handler]] for 4 effects can be created. */
    def &![Fx4 <: Stub](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  /** Provides environment for implementing [[Handler Handlers]], that handle 4 [[Effect Effects]] at once. */
  final class Combine4[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub, Fx4 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4) extends Combine(fx1, fx2, fx3, fx4):
    override type ThisEffect = fx1.type & fx2.type & fx3.type & fx4.type
    private[turbolift] override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature & fx4.ThisSignature
