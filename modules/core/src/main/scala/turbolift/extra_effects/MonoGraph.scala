package turbolift.extra_effects
import cats.Monoid
import turbolift.abstraction.{!!, Effect}


trait MonoGraphSig[U, K, V] {
  def empty(to: K): Unit !! U
  def incomingConst(to: K, value: V): Unit !! U
  def outgoingConst(from: K, value: V): Unit !! U
  def incoming(to: K, from: K): Unit !! U
  def incomings(to: K, froms: IterableOnce[K]): Unit !! U
  def outgoing(from: K, to: K): Unit !! U
  def outgoings(from: K, tos: IterableOnce[K]): Unit !! U
}


trait MonoGraph[K, V] extends Effect[MonoGraphSig[*, K, V]] {
  enclosing =>
  def empty(to: K): Unit !! this.type = embedFO(_.empty(to))
  def incomingConst(to: K, value: V): Unit !! this.type = embedFO(_.incomingConst(to, value))
  def outgoingConst(from: K, value: V): Unit !! this.type = embedFO(_.outgoingConst(from, value))
  def incoming(to: K, from: K): Unit !! this.type = embedFO(_.incoming(to, from))
  def incomings(to: K, froms: IterableOnce[K]): Unit !! this.type = embedFO(_.incomings(to, froms))
  def outgoing(from: K, to: K): Unit !! this.type = embedFO(_.outgoing(from, to))
  def outgoings(from: K, tos: IterableOnce[K]): Unit !! this.type = embedFO(_.outgoings(from, tos))

  final def at(k: K): AtApply = new AtApply(k)
  final class AtApply(k: K) {
    def empty = enclosing.empty(k)
    def incomingConst(value: V) = enclosing.incomingConst(k, value)
    def outgoingConst(value: V) = enclosing.outgoingConst(k, value)
    def incoming(from: K) = enclosing.incoming(k, from)
    def incomings(froms: IterableOnce[K]) = enclosing.incomings(k, froms)
    def outgoing(to: K) = enclosing.outgoing(k, to)
    def outgoings(tos: IterableOnce[K]) = enclosing.outgoings(k, tos)
  }

  def handler(implicit M: Monoid[V]): ThisIHandler[(Map[K, V], *)] = MonoGraphHandler[K, V, this.type](this)
  def handler(zero: V, combine: (V, V) => V): ThisIHandler[(Map[K, V], *)] = handler(Monoid.instance(zero, combine))
}
