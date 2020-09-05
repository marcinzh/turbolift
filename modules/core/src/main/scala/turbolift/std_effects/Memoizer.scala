package turbolift.std_effects
import cats.Id
import turbolift.abstraction.{!!, Effect}
import turbolift.std_handlers.DefaultMemoizerHandler


trait MemoizerSig[U, K, V] {
  def memo(fun: K => V !! U)(k: K): V !! U
  def snapshot: Map[K, V] !! U
}

trait Memoizer[K, V] extends Effect[MemoizerSig[?, K, V]] {
  final def memo[U](fun: K => V !! U)(k: K): V !! U with this.type = embedHO[U](_.memo(fun)(k))
  final def snapshot: Map[K, V] !! this.type = embedFO(_.snapshot)

  def handler: ThisHandler[Id] = DefaultMemoizerHandler[K, V, this.type](this)
}
