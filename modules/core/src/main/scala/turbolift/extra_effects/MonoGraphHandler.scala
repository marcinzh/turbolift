package turbolift.extra_effects
import scala.util.chaining._
import cats.Monoid
import turbolift.abstraction.!!
import turbolift.abstraction.Implicits._
import turbolift.std_effects.{WriterG, WriterGK}
import turbolift.abstraction.typeclass.Syntax._


object MonoGraphHandler {
  def apply[K, V, Fx <: MonoGraph[K, V]](fx: Fx)(implicit V: Monoid[V]): fx.ThisIHandler[(Map[K, V], *)] = {
    case object IncomingConst extends WriterG[Map, K, V]
    case object OutgoingConst extends WriterG[Map, K, V]
    case object Propagate extends WriterGK[Map, K, Set, K]
    type Fx3 = IncomingConst.type with OutgoingConst.type with Propagate.type

    new fx.Proxy[Fx3] {
      override def onOperation[U <: Fx3] = new MonoGraphSig[U, K, V] {
        override def empty(k: K): Unit !! U = IncomingConst.tell(k, V.empty)
        override def incomingConst(to: K, value: V): Unit !! U = IncomingConst.tell(to, value)
        override def outgoingConst(from: K, value: V): Unit !! U = OutgoingConst.tell(from, value)
        override def outgoing(from: K, to: K): Unit !! U = incoming(to, from)
        override def incoming(to: K, from: K): Unit !! U = Propagate.tell(from, to)
        override def incomings(to: K, froms: IterableOnce[K]): Unit !! U = froms.foreach_!!(incoming(to, _))
        override def outgoings(from: K, tos: IterableOnce[K]): Unit !! U = tos.foreach_!!(outgoing(from, _))
      }
    }
    .toHandler
    .provideWith(IncomingConst.handler ***! OutgoingConst.handler ***! Propagate.handler)
    .mapState { case ((in, out), prop) =>
      solve(
        in.withDefaultValue(V.empty),
        out.withDefaultValue(V.empty),
        prop.withDefaultValue(Set[K]()),
      )
    }
  }


  private def solve[K, V](inConst: Map[K, V], outConst: Map[K, V], propagate: Map[K, Set[K]])(implicit V: Monoid[V]): Map[K, V] = {
    def loop(solution: Map[K, V], dirty: Set[K]): Map[K, V] =
      if (dirty.isEmpty)
        solution
      else {
        val updatesCombined: Map[K, V] =
          (for {
            k <- dirty.iterator
            v = solution(k)
            k2 <- propagate(k).iterator
            kv = (k2, v)
          } yield kv)
          .foldLeft(Map[K, V]())(_ |+ _)

        updatesCombined.foldLeft((solution, Set[K]())) {
          case (accum, (k, v1)) =>
            val v0 = solution(k)
            val v = V.combine(v0, v1)
            if (v == v0)
              accum
            else {
              val (solution2, dirty2) = accum
              (solution2.updated(k, v), dirty2 + k)
            }
        }
        .pipe((loop(_, _)).tupled)
      }

    val domain = Set.empty[K] ++
      inConst.keysIterator ++
      outConst.keysIterator ++
      propagate.valuesIterator.flatten

    val initial = {
      val outConstCombined: Map[K, V] =
        (for {
          (k1, v) <- outConst.iterator
          k2 <- propagate(k1).iterator
          kv = (k2, v)
        } yield kv)
        .foldLeft(Map[K, V]())(_ |+ _)
        .withDefaultValue(V.empty)

      (for {
        k <- domain.iterator
        v1 = inConst(k)
        v2 = outConstCombined(k)
        v = V.combine(v1, v2)
        kv = (k, v)
      } yield kv)
      .toMap
    }

    loop(initial, domain)
  }
}
