package turbolift.internals.interpreter


/** State of stateless [[Interpreter]]. */
sealed abstract class Void

/** Singleton instance of [[Void]]. */
case object Void extends Void:
  private[internals] def as[S]: S = asInstanceOf[S]
  private[internals] val pair: (Void, Void) = (this, this)
  private[internals] def orElse(a: Any, b: Any): Any = if Void != a then a else b
