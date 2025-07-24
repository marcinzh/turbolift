package turbolift.effects
import scala.util.chaining._
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.typeclass.AccumZero
import turbolift.effects.{DependencyGraph, DependencyGraphSignature}
import turbolift.effects.{WriterEffectG, WriterEffectGK}


/** Badly needs better name.
 *
 * Constructs a directed, cyclic, immutable graph of nodes labelled with `K`.
 *
 * Each node holds a mutable (exception!) cell of value `V`.
 * It may be given initial value, or be initially empty.
 *
 * Each node can also specify a list of nodes it depends on its (by its `K` labels).
 * This forms directed edges in the graph.
 *
 * Each node can also specify a function, that would recompute its value `V`
 * from a list of arguments `V`, assuming they comie from the dependencies.
 *
 * The handler tracks changes of `V`s, recursively propagates them through dependency network,
 * recomputing `V`s as necessary, until fixed point is reached. The result is a `Map[K, V]`.
 */
trait DependencyGraphSignature[K, V] extends Signature:
  def empty(to: K): Unit !! ThisEffect
  def const(to: K, value: V): Unit !! ThisEffect
  def identity(to: K, from: K): Unit !! ThisEffect
  def unary(to: K, from: K)(f: V => V): Unit !! ThisEffect
  def binary(to: K, from1: K, from2: K)(f: (V, V) => V): Unit !! ThisEffect
  def variadic(to: K, froms: Vector[K])(f: Vector[V] => V): Unit !! ThisEffect


trait DependencyGraph[K, V] extends Effect[DependencyGraphSignature[K, V]] with DependencyGraphSignature[K, V]:
  enclosing =>
  final override def empty(to: K): Unit !! this.type = perform(_.empty(to))
  final override def const(to: K, value: V): Unit !! this.type = perform(_.const(to, value))
  final override def identity(to: K, from: K): Unit !! this.type = perform(_.identity(to, from))
  final override def unary(to: K, from: K)(f: V => V): Unit !! this.type = perform(_.unary(to, from)(f))
  final override def binary(to: K, from1: K, from2: K)(f: (V, V) => V): Unit !! this.type = perform(_.binary(to, from1, from2)(f))
  final override def variadic(to: K, froms: Vector[K])(f: Vector[V] => V): Unit !! this.type = perform(_.variadic(to, froms)(f))

  final def fold(to: K, froms: Vector[K], initial: V)(f: (V, V) => V): Unit !! this.type = variadic(to, froms)(_.fold(initial)(f))
  final def reduce(to: K, froms: Vector[K])(f: (V, V) => V): Unit !! this.type = variadic(to, froms)(_.reduce(f))

  final def at(k: K): AtApply = new AtApply(k)
  final class AtApply(k: K):
    def empty = enclosing.empty(k)
    def const(value: V) = enclosing.const(k, value)
    def identity(from: K) = enclosing.identity(k, from)
    def unary(from: K)(f: V => V) = enclosing.unary(k, from)(f)
    def binary(from1: K, from2: K)(f: (V, V) => V) = enclosing.binary(k, from1, from2)(f)
    def variadic(froms: Vector[K])(f: Vector[V] => V) = enclosing.variadic(k, froms)(f)
    def fold(froms: Vector[K], initial: V)(f: (V, V) => V) = enclosing.fold(k, froms, initial)(f)
    def reduce(froms: Vector[K])(f: (V, V) => V) = enclosing.reduce(k, froms)(f)


  /** Predefined handlers for this effect. */
  object handlers:
    def local(bottom: V): Handler.Id[(_, Map[K, V]), enclosing.type, Any] =
      type Solution = K => V

      case object Compute extends WriterEffectG[Map, K, Solution => V]
      case object Propagate extends WriterEffectGK[Map, K, Set, K]

      def computeConst(value: V): Solution => V = (_: Solution) => value
      val computeBottom = computeConst(bottom)

      new impl.Proxy[Compute.type & Propagate.type] with DependencyGraphSignature[K, V]:
        override def empty(to: K): Unit !! ThisEffect = Compute.tell(to, computeBottom)

        override def const(to: K, value: V): Unit !! ThisEffect = Compute.tell(to, computeConst(value))

        override def identity(to: K, from: K): Unit !! ThisEffect =
          Compute.tell(to, (sol: Solution) => sol(from)) &&!
          Propagate.tell(from, to)

        override def unary(to: K, from: K)(f: V => V): Unit !! ThisEffect =
          Compute.tell(to, (sol: Solution) => f(sol(from))) &&!
          Propagate.tell(from, to)

        override def binary(to: K, from1: K, from2: K)(f: (V, V) => V): Unit !! ThisEffect =
          Compute.tell(to, (sol: Solution) => f(sol(from1), sol(from2))) &&!
          Propagate.tell(from1, to) &&!
          Propagate.tell(from2, to)

        override def variadic(to: K, froms: Vector[K])(f: Vector[V] => V): Unit !! ThisEffect =
          Compute.tell(to, (sol: Solution) => f(froms.map(sol))) &&!
          froms.foreachEff(Propagate.tell(_, to))

      .toHandler
      .provideWith(Propagate.handler ***! Compute.handler(using AccumZero.forCollisionlessMap))
      .mapState: (prop, comp) =>
        solveDeps(
          bottom,
          comp.withDefaultValue(computeBottom),
          prop.withDefaultValue(Set.empty)
        ) 


object DependencyGraph:
  extension [K, V](thiz: DependencyGraph[K, V])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler(bottom: V): Handler.Id[(_, Map[K, V]), thiz.type, Any] = thiz.handlers.local(bottom)


private def solveDeps[K, V](bottom: V, compute: Map[K, (K => V) => V], propagate: Map[K, Set[K]]): Map[K, V] =
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

