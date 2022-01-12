package turbolift.extra_effects
import turbolift.abstraction.{!!, Effect, Signature}


trait CyclicMemoizerSig[K, V] extends Signature:
  def memo[U <: ThisEffect](f: K => V !! U)(k: K): (() => V) !@! U
  def get: Map[K, V] !@! ThisEffect


trait CyclicMemoizer[K, V] extends Effect[CyclicMemoizerSig[K, V]] with CyclicMemoizerSig[K, V]:
  final override def memo[U <: this.type](f: K => V !! U)(k: K): (() => V) !! U = impure(_.memo(f)(k))
  final override def get: Map[K, V] !! this.type = impure(_.get)

  final def apply[U <: this.type](f: K => V !! U): K => (() => V) !! U = memo(f)(_)

  final def fix[U <: this.type](f: (K => (() => V) !! U) => (K => V !! U)): K => (() => V) !! U =
    def recur(k: K): (() => V) !! U = memo(f(recur))(k)
    recur

  def handler: ThisIIdHandler = CyclicMemoizerHandler[K, V, this.type](this)
