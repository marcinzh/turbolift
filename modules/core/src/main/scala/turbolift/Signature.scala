package turbolift

/** Base trait for any user-defined effect signature.
 *
 *  Effect signature is a trait, where the effect's operations are declared as abstract methods. 
 *
 *  Typically, a custom defined [[Signature]] is 1-1 paired with a custom defined [[Effect]].
 *
 *  Effect signatures play the same role as:
 *  - Algebras in Tagless Final.
 *  - Services in ZIO.
 *
 *  Example:
 *  {{{
 *  import turbolift.Signature
 *
 *  trait GoogleSignature extends Signature:
 *    def countPicturesOf(topic: String): Int !@! ThisEffect
 *  }}}
 *
 *
 *

 *  Effect operations must:
 *  - Be defined as abstract methods.
 *  - Have their return types of shape: `X !@! ThisEffect`, for some type `X`.
 *
 *  It may be helpful to think of `!@![_, ThisEffect]` as analogous to `F[_]` in Tagless Final.
 *  Except in Turbolift, it's meant to be used in the **return type only**. 
 *
 *  In the parameters, plain `!!` should be used. Example of scoped operation:
 *
 *  {{{
 *  trait ErrorSignature[E] extends Signature:
 *    def catchError[A, U <: ThisEffect](scope: A !! U)(f: E => A !! U): A !@! U
 *  }}}
 *
 */

trait Signature extends AnyRef:

/**
  Abstract type that must be used in definitions of effect's operations.

  From the perspective of effect's user, `!@!` is just an alias of [[Computation !!]]. The final-override happens in [[Effect]].

  From the perspective of handler, `!@!` definition is enriched in a way depending on the chosen [[internals.interpreter.Interpreter Interpreter]].
*/
  type !@![A, U]

/**
  Abstract type that must be used in definitions of effect's operations.
  
  From the perspective of effect's user, [[ThisEffect]] is just an alias of `this.type`. The final-override happens in [[Effect]].

  From the perspective of handler, [[ThisEffect]] definition is enriched in a way depending on the chosen [[internals.interpreter.Interpreter Interpreter]].
*/
  type ThisEffect
