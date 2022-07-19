package turbolift

/** Base trait for any user-defined effect signature.
 *
 *  Effect signature is a trait, where effect's operations are declared as abstract methods. 
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
 *  Effect operations must:
 *  - Be defined as abstract methods.
 *  - Have their return types of shape: `X !@! ThisEffect`, for some type `X`.
 *
 *  TODO: explain how higher order operations definitions differ.
 *
 *  It may be helpful to think of `!@![_, ThisEffect]` as a thing analogous to `F[_]` in Tagless Final.
 *
 *  Typically, a custom defined [[Signature]] is 1-1 paired with a custom defined [[Effect]].
 */

trait Signature extends AnyRef:

/**
  Abstract type that must be used in definitions of effect's operations.

  From the perspective of effect's user, `!@!` is just an alias of [[Computation !!]]. The final-override happens in [[Effect]].

  From the perspective of handler's implementor, `!@!` definition is enriched in a way depending on the chosen [[internals.interpreter.InterpreterCases Interpreter]].
*/
  type !@![A, U]

/**
  Abstract type that must be used in definitions of effect's operations.
  
  From the perspective of effect's user, [[ThisEffect]] is just an alias of `this.type`. The final-override happens in [[Effect]].

  From the perspective of handler's implementor, [[ThisEffect]] definition is enriched in a way depending on the chosen [[internals.interpreter.InterpreterCases Interpreter]].
*/
  type ThisEffect
