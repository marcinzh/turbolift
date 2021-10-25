package turbolift.extra_effects
import turbolift.abstraction.{!!, Effect}


trait PolyGraphSig[U, K, V]:
  def empty(to: K): Unit !! U
  def const(to: K, value: V): Unit !! U
  def identity(to: K, from: K): Unit !! U
  def unary(to: K, from: K)(f: V => V): Unit !! U
  def binary(to: K, from1: K, from2: K)(f: (V, V) => V): Unit !! U
  def variadic(to: K, froms: Vector[K])(f: Vector[V] => V): Unit !! U


trait PolyGraph[K, V] extends Effect[PolyGraphSig[_, K, V]]:
  enclosing =>
  final def empty(to: K): Unit !! this.type = impureFO(_.empty(to))
  final def const(to: K, value: V): Unit !! this.type = impureFO(_.const(to, value))
  final def identity(to: K, from: K): Unit !! this.type = impureFO(_.identity(to, from))
  final def unary(to: K, from: K)(f: V => V): Unit !! this.type = impureFO(_.unary(to, from)(f))
  final def binary(to: K, from1: K, from2: K)(f: (V, V) => V): Unit !! this.type = impureFO(_.binary(to, from1, from2)(f))
  final def variadic(to: K, froms: Vector[K])(f: Vector[V] => V): Unit !! this.type = impureFO(_.variadic(to, froms)(f))
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

  def handler: V => ThisIHandler[(Map[K, V], _)] = PolyGraphHandler.apply[K, V, this.type](this)
