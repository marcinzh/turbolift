package turbolift.handlers
import scala.util.chaining._
import turbolift.!!
import turbolift.Extensions._
import turbolift.typeclass.AccumZero
import turbolift.effects.{PolyGraph, PolyGraphSignature}
import turbolift.effects.{WriterG, WriterGK}


extension [K, V](fx: PolyGraph[K, V])
  def polyGraphHandler(bottom: V): fx.ThisHandler.FromId.Free[(_, Map[K, V])] =
    type Solution = K => V

    case object Compute extends WriterG[Map, K, Solution => V]
    case object Propagate extends WriterGK[Map, K, Set, K]

    def computeConst(value: V): Solution => V = (_: Solution) => value
    val computeBottom = computeConst(bottom)

    new fx.impl.Proxy[Compute.type & Propagate.type] with PolyGraphSignature[K, V]:
      override def empty(to: K): Unit !@! ThisEffect = Compute.tell(to, computeBottom)

      override def const(to: K, value: V): Unit !@! ThisEffect = Compute.tell(to, computeConst(value))

      override def identity(to: K, from: K): Unit !@! ThisEffect =
        Compute.tell(to, (sol: Solution) => sol(from)) &&!
        Propagate.tell(from, to)

      override def unary(to: K, from: K)(f: V => V): Unit !@! ThisEffect =
        Compute.tell(to, (sol: Solution) => f(sol(from))) &&!
        Propagate.tell(from, to)

      override def binary(to: K, from1: K, from2: K)(f: (V, V) => V): Unit !@! ThisEffect =
        Compute.tell(to, (sol: Solution) => f(sol(from1), sol(from2))) &&!
        Propagate.tell(from1, to) &&!
        Propagate.tell(from2, to)

      override def variadic(to: K, froms: Vector[K])(f: Vector[V] => V): Unit !@! ThisEffect =
        Compute.tell(to, (sol: Solution) => f(froms.map(sol))) &&!
        froms.foreachEff(Propagate.tell(_, to))

    .toHandler
    .provideWith(Propagate.handler ***! Compute.handler(AccumZero.collisionlessMap))
    .mapState { case (prop, comp) =>
      solvePoly(
        bottom,
        comp.withDefaultValue(computeBottom),
        prop.withDefaultValue(Set.empty)
      ) 
    }


private def solvePoly[K, V](bottom: V, compute: Map[K, (K => V) => V], propagate: Map[K, Set[K]]): Map[K, V] =
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

