package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.handlers.acyclicMemoizerHandler


trait AcyclicMemoizerSignature[K, V] extends Signature:
  def memo[U <: ThisEffect](f: K => V !! U)(k: K): V !@! U
  def domain: Set[K] !@! ThisEffect
  def toMap: Map[K, V] !@! ThisEffect
  @deprecated final def get = toMap


trait AcyclicMemoizer[K, V] extends Effect[AcyclicMemoizerSignature[K, V]] with AcyclicMemoizerSignature[K, V]:
  final override def memo[U <: this.type](f: K => V !! U)(k: K): V !! U = perform(_.memo(f)(k))
  final override def domain: Set[K] !! this.type = perform(_.domain)
  final override def toMap: Map[K, V] !! this.type = perform(_.toMap)

  final def apply[U <: this.type](f: K => V !! U): K => V !! U = memo(f)(_)

  final def fix[U] = new FixApply[U]
  final class FixApply[U]:
    def apply[U2 <: U & AcyclicMemoizer.this.type](f: (K => V !! U2) => (K => V !! U2)): K => V !! U2 =
      def recur(k: K): V !! U2 = memo(f(recur))(k)
      recur

  /** Default handler for this effect. */
  def handler: ThisHandler[Identity, Identity, Any] = this.acyclicMemoizerHandler
