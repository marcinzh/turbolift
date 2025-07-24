package turbolift.effects
import scala.util.chaining._
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.typeclass.PlusZero
import turbolift.typeclass.Syntax._
import turbolift.effects.{WriterEffectG, WriterEffectGK}


/** Badly needs better name.
 *
 * Constructs a directed, cyclic, immutable graph of nodes labelled with `K`.
 * Each node holds a mutable (exception!) cell of value `V`.
 * It may be given initial value, or be initially empty.
 *
 * Directed edges can be added by specifying pairs of node labels. 
 *
 * The `V` type is required to be a monoid.
 * The value `V` of a node is assumed to be recomputeable from
 * the monoidal sum of `V`s from nodes reachable directly by incoming edges.
 *
 * The handler tracks changes of `V`s, recursively propagates them through dependency network,
 * recomputing `V`s as necessary, until fixed point is reached. The result is a `Map[K, V]`.
 */
trait MonoidalDependencyGraphSignature[K, V] extends Signature:
  def empty(to: K): Unit !! ThisEffect
  def incomingConst(to: K, value: V): Unit !! ThisEffect
  def outgoingConst(from: K, value: V): Unit !! ThisEffect
  def incoming(to: K, from: K): Unit !! ThisEffect
  def incomings(to: K, froms: IterableOnce[K]): Unit !! ThisEffect
  def outgoing(from: K, to: K): Unit !! ThisEffect
  def outgoings(from: K, tos: IterableOnce[K]): Unit !! ThisEffect


trait MonoidalDependencyGraph[K, V] extends Effect[MonoidalDependencyGraphSignature[K, V]] with MonoidalDependencyGraphSignature[K, V]:
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


  /** Predefined handlers for this effect. */
  object handlers:
    def local(using V: PlusZero[V]): Handler.Id[(_, Map[K, V]), enclosing.type, Any] =
      case object IncomingConst extends WriterEffectG[Map, K, V]
      case object OutgoingConst extends WriterEffectG[Map, K, V]
      case object Propagate extends WriterEffectGK[Map, K, Set, K]
      type Fx3 = IncomingConst.type & OutgoingConst.type & Propagate.type

      new impl.Proxy[Fx3] with MonoidalDependencyGraphSignature[K, V]:
        override def empty(k: K): Unit !! ThisEffect = IncomingConst.tell(k, V.zero)
        override def incomingConst(to: K, value: V): Unit !! ThisEffect = IncomingConst.tell(to, value)
        override def outgoingConst(from: K, value: V): Unit !! ThisEffect = OutgoingConst.tell(from, value)
        override def outgoing(from: K, to: K): Unit !! ThisEffect = incoming(to, from)
        override def incoming(to: K, from: K): Unit !! ThisEffect = Propagate.tell(from, to)
        override def incomings(to: K, froms: IterableOnce[K]): Unit !! ThisEffect = froms.foreachEff(Propagate.tell(_, to))
        override def outgoings(from: K, tos: IterableOnce[K]): Unit !! ThisEffect = tos.foreachEff(Propagate.tell(from, _))

      .toHandler
      .provideWith(IncomingConst.handler ***! OutgoingConst.handler ***! Propagate.handler)
      .mapState: (in, out, prop) =>
        solveMonoidalDeps(
          in.withDefaultValue(V.zero),
          out.withDefaultValue(V.zero),
          prop.withDefaultValue(Set[K]()),
        )


    def local(zero: V, plus: (V, V) => V): Handler.Id[(_, Map[K, V]), enclosing.type, Any] = local(using PlusZero.instance(zero, plus))


object MonoidalDependencyGraph:
  extension [K, V](thiz: MonoidalDependencyGraph[K, V])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler(using V: PlusZero[V]): Handler.Id[(_, Map[K, V]), thiz.type, Any] = thiz.handlers.local


private def solveMonoidalDeps[K, V](inConst: Map[K, V], outConst: Map[K, V], propagate: Map[K, Set[K]])(using V: PlusZero[V]): Map[K, V] =
  def loop(solution: Map[K, V], dirty: Set[K]): Map[K, V] =
    if dirty.isEmpty
    then solution
    else
      val updatesCombined: Map[K, V] =
        (for
          k <- dirty.iterator
          v = solution(k)
          k2 <- propagate(k).iterator
          kv = (k2, v)
        yield kv)
        .foldLeft(Map[K, V]())(_ |+ _)

      updatesCombined.foldLeft((solution, Set[K]())) {
        case (accum, (k, v1)) =>
          val v0 = solution(k)
          val v = v0 |+| v1
          if v == v0
          then accum
          else
            val (solution2, dirty2) = accum
            (solution2.updated(k, v), dirty2 + k)
      }
      .pipe((loop(_, _)).tupled)

  val domain = Set.empty[K] ++
    inConst.keysIterator ++
    outConst.keysIterator ++
    propagate.valuesIterator.flatten

  val initial =
    val outConstCombined: Map[K, V] =
      (for
        (k1, v) <- outConst.iterator
        k2 <- propagate(k1).iterator
        kv = (k2, v)
      yield kv)
      .foldLeft(Map[K, V]())(_ |+ _)
      .withDefaultValue(V.zero)

    (for
      k <- domain.iterator
      v1 = inConst(k)
      v2 = outConstCombined(k)
      v = v1 |+| v2
      kv = (k, v)
    yield kv)
    .toMap

  loop(initial, domain)
