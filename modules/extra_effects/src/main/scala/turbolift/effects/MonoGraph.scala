package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.Extensions._
import turbolift.handlers.monoGraphHandler
import turbolift.typeclass.PlusZero


trait MonoGraphSignature[K, V] extends Signature:
  def empty(to: K): Unit !! ThisEffect
  def incomingConst(to: K, value: V): Unit !! ThisEffect
  def outgoingConst(from: K, value: V): Unit !! ThisEffect
  def incoming(to: K, from: K): Unit !! ThisEffect
  def incomings(to: K, froms: IterableOnce[K]): Unit !! ThisEffect
  def outgoing(from: K, to: K): Unit !! ThisEffect
  def outgoings(from: K, tos: IterableOnce[K]): Unit !! ThisEffect


trait MonoGraph[K, V] extends Effect[MonoGraphSignature[K, V]] with MonoGraphSignature[K, V]:
  enclosing =>
  def empty(to: K): Unit !! this.type = perform(_.empty(to))
  def incomingConst(to: K, value: V): Unit !! this.type = perform(_.incomingConst(to, value))
  def outgoingConst(from: K, value: V): Unit !! this.type = perform(_.outgoingConst(from, value))
  def incoming(to: K, from: K): Unit !! this.type = perform(_.incoming(to, from))
  def incomings(to: K, froms: IterableOnce[K]): Unit !! this.type = perform(_.incomings(to, froms))
  def outgoing(from: K, to: K): Unit !! this.type = perform(_.outgoing(from, to))
  def outgoings(from: K, tos: IterableOnce[K]): Unit !! this.type = perform(_.outgoings(from, tos))

  final def at(k: K): AtApply = new AtApply(k)
  final class AtApply(k: K):
    def empty = enclosing.empty(k)
    def incomingConst(value: V) = enclosing.incomingConst(k, value)
    def outgoingConst(value: V) = enclosing.outgoingConst(k, value)
    def incoming(from: K) = enclosing.incoming(k, from)
    def incomings(froms: IterableOnce[K]) = enclosing.incomings(k, froms)
    def outgoing(to: K) = enclosing.outgoing(k, to)
    def outgoings(tos: IterableOnce[K]) = enclosing.outgoings(k, tos)

  /** Predefined handler for this effect. */
  def handler(using M: PlusZero[V]): ThisHandler[Identity, (_, Map[K, V]), Any] = this.monoGraphHandler

  /** Predefined handler for this effect. */
  def handler(zero: V, plus: (V, V) => V): ThisHandler[Identity, (_, Map[K, V]), Any] = handler(using PlusZero.instance(zero, plus))
