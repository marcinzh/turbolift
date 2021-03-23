package turbolift.extra_effects
import turbolift.abstraction.{!!, Effect}


trait PolyGraphSig[U, K, V] {
  def empty(to: K): Unit !! U
  def const(to: K, value: V): Unit !! U
  def identity(to: K, from: K): Unit !! U
  def unary(to: K, from: K)(fun: V => V): Unit !! U
  def binary(to: K, from1: K, from2: K)(fun: (V, V) => V): Unit !! U
  def variadic(to: K, froms: Vector[K])(fun: Vector[V] => V): Unit !! U
}


trait PolyGraph[K, V] extends Effect[PolyGraphSig[*, K, V]] {
  enclosing =>
  final def empty(to: K): Unit !! this.type = embedFO(_.empty(to))
  final def const(to: K, value: V): Unit !! this.type = embedFO(_.const(to, value))
  final def identity(to: K, from: K): Unit !! this.type = embedFO(_.identity(to, from))
  final def unary(to: K, from: K)(fun: V => V): Unit !! this.type = embedFO(_.unary(to, from)(fun))
  final def binary(to: K, from1: K, from2: K)(fun: (V, V) => V): Unit !! this.type = embedFO(_.binary(to, from1, from2)(fun))
  final def variadic(to: K, froms: Vector[K])(fun: Vector[V] => V): Unit !! this.type = embedFO(_.variadic(to, froms)(fun))
  final def fold(to: K, froms: Vector[K], initial: V)(fun: (V, V) => V): Unit !! this.type = variadic(to, froms)(_.fold(initial)(fun))
  final def reduce(to: K, froms: Vector[K])(fun: (V, V) => V): Unit !! this.type = variadic(to, froms)(_.reduce(fun))

  final def at(k: K): AtApply = new AtApply(k)
  final class AtApply(k: K) {
    def empty = enclosing.empty(k)
    def const(value: V) = enclosing.const(k, value)
    def identity(from: K) = enclosing.identity(k, from)
    def unary(from: K)(fun: V => V) = enclosing.unary(k, from)(fun)
    def binary(from1: K, from2: K)(fun: (V, V) => V) = enclosing.binary(k, from1, from2)(fun)
    def variadic(froms: Vector[K])(fun: Vector[V] => V) = enclosing.variadic(k, froms)(fun)
    def fold(froms: Vector[K], initial: V)(fun: (V, V) => V) = enclosing.fold(k, froms, initial)(fun)
    def reduce(froms: Vector[K])(fun: (V, V) => V) = enclosing.reduce(k, froms)(fun)
  }

  def handler: V => ThisIHandler[(Map[K, V], *)] = PolyGraphHandler.apply[K, V, this.type](this)
}
