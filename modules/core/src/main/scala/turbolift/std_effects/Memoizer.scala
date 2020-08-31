package turbolift.std_effects
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.std_handlers.DefaultMemoizerHandler


trait MemoizerSig[U, K, V] extends Signature[U] {
  def memo(fun: K => V !! U)(k: K): V !! U
  def snapshot: Map[K, V] !! U
}

trait Memoizer[K, V] extends Effect[MemoizerSig[?, K, V]] {
  final def memo[U](fun: K => V !! U)(k: K): V !! U with this.type = encodeHO[U](_.memo(fun)(k))
  final def snapshot: Map[K, V] !! this.type = encodeFO(_.snapshot)

  val handler = DefaultMemoizerHandler[K, V, this.type](this)
}
