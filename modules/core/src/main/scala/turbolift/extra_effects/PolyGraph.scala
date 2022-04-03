package turbolift.extra_effects
import turbolift.{!!, Effect, Signature}
import turbolift.extra_effects.default_handlers.PolyGraphHandler


trait PolyGraphSig[K, V] extends Signature:
  def empty(to: K): Unit !@! ThisEffect
  def const(to: K, value: V): Unit !@! ThisEffect
  def identity(to: K, from: K): Unit !@! ThisEffect
  def unary(to: K, from: K)(f: V => V): Unit !@! ThisEffect
  def binary(to: K, from1: K, from2: K)(f: (V, V) => V): Unit !@! ThisEffect
  def variadic(to: K, froms: Vector[K])(f: Vector[V] => V): Unit !@! ThisEffect


trait PolyGraph[K, V] extends Effect[PolyGraphSig[K, V]] with PolyGraphSig[K, V]:
  enclosing =>
  final override def empty(to: K): Unit !! this.type = operate(_.empty(to))
  final override def const(to: K, value: V): Unit !! this.type = operate(_.const(to, value))
  final override def identity(to: K, from: K): Unit !! this.type = operate(_.identity(to, from))
  final override def unary(to: K, from: K)(f: V => V): Unit !! this.type = operate(_.unary(to, from)(f))
  final override def binary(to: K, from1: K, from2: K)(f: (V, V) => V): Unit !! this.type = operate(_.binary(to, from1, from2)(f))
  final override def variadic(to: K, froms: Vector[K])(f: Vector[V] => V): Unit !! this.type = operate(_.variadic(to, froms)(f))

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

  def handler: V => ThisHandler.Free[(Map[K, V], _)] = PolyGraphHandler.apply[K, V, this.type](this)
