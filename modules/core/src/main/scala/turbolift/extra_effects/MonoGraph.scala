package turbolift.extra_effects
import cats.Monoid
import turbolift.{!!, Effect, Signature}
import turbolift.extra_effects.default_handlers.MonoGraphHandler


trait MonoGraphSig[K, V] extends Signature:
  def empty(to: K): Unit !@! ThisEffect
  def incomingConst(to: K, value: V): Unit !@! ThisEffect
  def outgoingConst(from: K, value: V): Unit !@! ThisEffect
  def incoming(to: K, from: K): Unit !@! ThisEffect
  def incomings(to: K, froms: IterableOnce[K]): Unit !@! ThisEffect
  def outgoing(from: K, to: K): Unit !@! ThisEffect
  def outgoings(from: K, tos: IterableOnce[K]): Unit !@! ThisEffect


trait MonoGraph[K, V] extends Effect[MonoGraphSig[K, V]] with MonoGraphSig[K, V]:
  enclosing =>
  def empty(to: K): Unit !! this.type = operate(_.empty(to))
  def incomingConst(to: K, value: V): Unit !! this.type = operate(_.incomingConst(to, value))
  def outgoingConst(from: K, value: V): Unit !! this.type = operate(_.outgoingConst(from, value))
  def incoming(to: K, from: K): Unit !! this.type = operate(_.incoming(to, from))
  def incomings(to: K, froms: IterableOnce[K]): Unit !! this.type = operate(_.incomings(to, froms))
  def outgoing(from: K, to: K): Unit !! this.type = operate(_.outgoing(from, to))
  def outgoings(from: K, tos: IterableOnce[K]): Unit !! this.type = operate(_.outgoings(from, tos))

  final def at(k: K): AtApply = new AtApply(k)
  final class AtApply(k: K):
    def empty = enclosing.empty(k)
    def incomingConst(value: V) = enclosing.incomingConst(k, value)
    def outgoingConst(value: V) = enclosing.outgoingConst(k, value)
    def incoming(from: K) = enclosing.incoming(k, from)
    def incomings(froms: IterableOnce[K]) = enclosing.incomings(k, froms)
    def outgoing(to: K) = enclosing.outgoing(k, to)
    def outgoings(tos: IterableOnce[K]) = enclosing.outgoings(k, tos)

  def handler(implicit M: Monoid[V]): ThisHandler.Free[(Map[K, V], _)] = MonoGraphHandler[K, V, this.type](this)
  def handler(zero: V, combine: (V, V) => V): ThisHandler.Free[(Map[K, V], _)] = handler(Monoid.instance(zero, combine))
