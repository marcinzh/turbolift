package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.handlers.cyclicMemoizerHandler


trait CyclicMemoizerSignature[K, V] extends Signature:
  def memo(k: K): (() => V) !! ThisEffect
  def domain: Set[K] !! ThisEffect
  def toMap: Map[K, V] !! ThisEffect


trait CyclicMemoizerEffect[K, V] extends Effect[CyclicMemoizerSignature[K, V]] with CyclicMemoizerSignature[K, V]:
  enclosing =>
  final override def memo(k: K): (() => V) !! this.type = perform(_.memo(k))
  final override def domain: Set[K] !! this.type = perform(_.domain)
  final override def toMap: Map[K, V] !! this.type = perform(_.toMap)

  final def apply(k: K): (() => V) !! this.type = memo(k)

  /** Predefined handlers for this effect. */
  object handlers:
    def default[U](f: K => V !! (U & enclosing.type)): ThisHandler[Identity, Identity, U] = enclosing.cyclicMemoizerHandler[U](f)


trait CyclicMemoizer[K, V] extends CyclicMemoizerEffect[K, V]:
  export handlers.{default => handler}


object CyclicMemoizer:
  trait Fix[K, V, U] extends CyclicMemoizerEffect[K, V]:
    val handler: ThisHandler[Identity, Identity, U]

  def fix[K, V, U](f: (fx: Fix[K, V, U]) => K => V !! (U & fx.type)): Fix[K, V, U] = new:
    override val handler: ThisHandler[Identity, Identity, U] = handlers.default[U](f(this))
