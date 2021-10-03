package turbolift.extra_effects
import scala.util.chaining._
import turbolift.abstraction.!!
import turbolift.abstraction.Implicits._
import turbolift.abstraction.typeclass.AccumZero
import turbolift.std_effects.{WriterG, WriterGK}


object PolyGraphHandler:
  def apply[K, V, Fx <: PolyGraph[K, V]](fx: Fx)(bottom: V): fx.ThisIHandler[(Map[K, V], _)] =
    type Solution = K => V

    case object Compute extends WriterG[Map, K, Solution => V]
    case object Propagate extends WriterGK[Map, K, Set, K]

    def computeConst(value: V): Solution => V = (_: Solution) => value
    val computeBottom = computeConst(bottom)

    new fx.Proxy[Compute.type with Propagate.type]:
      override def onOperation[U <: Compute.type with Propagate.type] = new PolyGraphSig[U, K, V]:
        override def empty(to: K): Unit !! U = Compute.tell(to, computeBottom)

        override def const(to: K, value: V): Unit !! U = Compute.tell(to, computeConst(value))

        override def identity(to: K, from: K): Unit !! U =
          Compute.tell(to, (sol: Solution) => sol(from)) &!
          Propagate.tell(from, to)

        override def unary(to: K, from: K)(fun: V => V): Unit !! U =
          Compute.tell(to, (sol: Solution) => fun(sol(from))) &!
          Propagate.tell(from, to)

        override def binary(to: K, from1: K, from2: K)(fun: (V, V) => V): Unit !! U =
          Compute.tell(to, (sol: Solution) => fun(sol(from1), sol(from2))) &!
          Propagate.tell(from1, to) &!
          Propagate.tell(from2, to)

        override def variadic(to: K, froms: Vector[K])(fun: Vector[V] => V): Unit !! U =
          Compute.tell(to, (sol: Solution) => fun(froms.map(sol))) &!
          froms.foreach_!!(Propagate.tell(_, to))

    .toHandler
    .provideWith(Propagate.handler ***! Compute.handler(AccumZero.collisionlessMap))
    .mapState { case (prop, comp) =>
      solve(
        bottom,
        comp.withDefaultValue(computeBottom),
        prop.withDefaultValue(Set.empty)
      ) 
    }


  private def solve[K, V](bottom: V, compute: Map[K, (K => V) => V], propagate: Map[K, Set[K]]): Map[K, V] =
    def loop(solution: Map[K, V], dirty: Iterable[K]): Map[K, V] =
      if dirty.isEmpty
      then solution
      else
        dirty.iterator.foldLeft((solution, Set[K]())) {
          case (accum, k) =>
            val v = compute(k)(solution)
            if v == solution(k)
            then accum
            else
              val (solution2, dirty2) = accum
              (solution2.updated(k, v), dirty2 ++ propagate(k))
        }
        .pipe((loop(_, _)).tupled)

    val domain = Set[K]() ++ compute.keysIterator ++ propagate.keysIterator
    val initial = Map[K, V]().withDefaultValue(bottom)
    val m = loop(initial, domain)
    m ++ domain.iterator.filter(!m.contains(_)).map(k => k -> bottom)

