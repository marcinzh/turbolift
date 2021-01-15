package turbolift.extra_effects
import cats.Id
import turbolift.abstraction.{!!, Effect}


trait CyclicMemoizerSig[U, K, V] {
  def memo(f: K => V !! U)(k: K): (() => V) !! U
  def get: Map[K, V] !! U
}

trait CyclicMemoizer[K, V] extends Effect[CyclicMemoizerSig[*, K, V]] {
  final def memo[U](f: K => V !! U)(k: K): (() => V) !! U with this.type = embedHO[U](_.memo(f)(k))
  final def get: Map[K, V] !! this.type = embedFO(_.get)

  final def apply[U](f: K => V !! U)(k: K) = memo(f)(k)

  def handler: ThisIHandler[Id] = CyclicMemoizerHandler[K, V, this.type](this)
}
