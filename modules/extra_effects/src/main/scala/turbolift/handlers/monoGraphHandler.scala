package turbolift.handlers
import scala.util.chaining._
import turbolift.!!
import turbolift.Extensions._
import turbolift.typeclass.PlusZero
import turbolift.typeclass.Syntax._
import turbolift.effects.{MonoGraph, MonoGraphSignature}
import turbolift.effects.{WriterG, WriterGK}


extension [K, V](fx: MonoGraph[K, V])
  def monoGraphHandler(using V: PlusZero[V]): fx.ThisHandler[Identity, (_, Map[K, V]), Any] =
    case object IncomingConst extends WriterG[Map, K, V]
    case object OutgoingConst extends WriterG[Map, K, V]
    case object Propagate extends WriterGK[Map, K, Set, K]
    type Fx3 = IncomingConst.type & OutgoingConst.type & Propagate.type

    new fx.impl.Proxy[Fx3] with MonoGraphSignature[K, V]:
      override def empty(k: K): Unit !! ThisEffect = IncomingConst.tell(k, V.zero)
      override def incomingConst(to: K, value: V): Unit !! ThisEffect = IncomingConst.tell(to, value)
      override def outgoingConst(from: K, value: V): Unit !! ThisEffect = OutgoingConst.tell(from, value)
      override def outgoing(from: K, to: K): Unit !! ThisEffect = incoming(to, from)
      override def incoming(to: K, from: K): Unit !! ThisEffect = Propagate.tell(from, to)
      override def incomings(to: K, froms: IterableOnce[K]): Unit !! ThisEffect = froms.foreachEff(Propagate.tell(_, to))
      override def outgoings(from: K, tos: IterableOnce[K]): Unit !! ThisEffect = tos.foreachEff(Propagate.tell(from, _))

    .toHandler
    .provideWith(IncomingConst.handler ***! OutgoingConst.handler ***! Propagate.handler)
    .mapState { case (in, out, prop) =>
      solveMono(
        in.withDefaultValue(V.zero),
        out.withDefaultValue(V.zero),
        prop.withDefaultValue(Set[K]()),
      )
    }


private def solveMono[K, V](inConst: Map[K, V], outConst: Map[K, V], propagate: Map[K, Set[K]])(using V: PlusZero[V]): Map[K, V] =
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
