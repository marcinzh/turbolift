package turbolift.interpreter
import turbolift.!!


/** Degenerate state of stateless [[Interpreter]]. 
 *
 *Like `Unit`, but takes 0 bytes of storage in fiber's state. Do not use explicitly.
 */
sealed abstract class Void

/** Singleton instance of [[Void]]. */
case object Void extends Void:
  private[turbolift] def as[S]: S = asInstanceOf[S]
  private[turbolift] val pair: (Void, Void) = (this, this)
  private[turbolift] val pure: Void !! Any = !!.pure(Void)
  // private[turbolift] def orElse(a: Any, b: Any): Any = if Void != a then a else b
