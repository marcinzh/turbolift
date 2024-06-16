package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.handlers.acyclicMemoizerHandler2


trait AcyclicMemoizerSignature[K, V] extends Signature:
  def memo(k: K): V !! ThisEffect
  def domain: Set[K] !! ThisEffect
  def toMap: Map[K, V] !! ThisEffect


trait AcyclicMemoizerEffect[K, V] extends Effect[AcyclicMemoizerSignature[K, V]] with AcyclicMemoizerSignature[K, V]:
  enclosing =>
  final override def memo(k: K): V !! this.type = perform(_.memo(k))
  final override def domain: Set[K] !! this.type = perform(_.domain)
  final override def toMap: Map[K, V] !! this.type = perform(_.toMap)

  final def apply(k: K): V !! this.type = memo(k)

  /** Predefined handlers for this effect. */
  object handlers:
    def default[U](f: K => V !! (U & enclosing.type)): ThisHandler[Identity, Identity, U] = enclosing.acyclicMemoizerHandler2[U](f)


trait AcyclicMemoizer[K, V] extends AcyclicMemoizerEffect[K, V]:
  export handlers.{default => handler}


object AcyclicMemoizer:
  trait Fix[K, V, U] extends AcyclicMemoizerEffect[K, V]:
    val handler: ThisHandler[Identity, Identity, U]

  def fix[K, V, U](f: (fx: Fix[K, V, U]) => K => V !! (U & fx.type)): Fix[K, V, U] = new:
    override val handler: ThisHandler[Identity, Identity, U] = handlers.default[U](f(this))
