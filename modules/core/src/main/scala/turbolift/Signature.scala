package turbolift

/** Base trait for any user-defined effect signature.
Used to define custom Algebra/Service/DSL.
 *
 *  Example:
 *  {{{
 *  import turbolift.Signature
 *
 *  trait GoogleSignature extends Signature:
 *    def countPicturesOf(topic: String): Int !@! ThisEffect
 *  }}}
 *
 *  See [Defining your own effects and handlers](https://marcinzh.github.io/turbolift/custom/index.html).
 */

trait Signature extends AnyRef:

/**
  Abstract type that must be used in definitions of effect's operations. 

  Becomes an alias of [[Computation]] (i.e. same as `!!`), once the signature is inherited from `Effect`.
*/
  type !@![A, U]

/**
  Abstract type that must be used in definitions of effect's operations.
  
  Becomes an alias of `this.type`, once the signature is inherited from `Effect`.
*/
  type ThisEffect
