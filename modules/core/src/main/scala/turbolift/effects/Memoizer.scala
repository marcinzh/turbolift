package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.Extensions._
import turbolift.handlers.{memoizerHandler_local, memoizerHandler_shared}


/** Memoizes a recursive, effectful function.
 *
 *  Use the `memo` operation in places, where you'd normally want to invoke the function.
 *  Provide the actual function as a parameter to handler.
 */

trait MemoizerSignature[K, V] extends Signature:
  def memo(k: K): V !! ThisEffect
  def domain: Set[K] !! ThisEffect
  def toMap: Map[K, V] !! ThisEffect


trait MemoizerEffect[K, V] extends Effect[MemoizerSignature[K, V]] with MemoizerSignature[K, V]:
  enclosing =>
  final override def memo(k: K): V !! this.type = perform(_.memo(k))
  final override def domain: Set[K] !! this.type = perform(_.domain)
  final override def toMap: Map[K, V] !! this.type = perform(_.toMap)

  final def apply(k: K): V !! this.type = memo(k)

  /** Predefined handlers for this effect. */
  object handlers:
    def local[U](f: K => V !! (U & enclosing.type)): ThisHandler[Identity, Identity, U] = enclosing.memoizerHandler_local[U](f)
    def shared[U <: IO](f: K => V !! (U & enclosing.type)): ThisHandler[Identity, Identity, U] = enclosing.memoizerHandler_shared[U](f)


trait Memoizer[K, V] extends MemoizerEffect[K, V]:
  export handlers.{local => handler}


//@#@TODO `fix` syntax doesn't work in Scala 3.6.3
/*
object Memoizer:
  trait Fix[K, V, U] extends MemoizerEffect[K, V]:
    val handler: ThisHandler[Identity, Identity, U]

  def fix[K, V, U](f: (fx: Fix[K, V, U]) => K => V !! (U & fx.type)): Fix[K, V, U] = new:
    override val handler: ThisHandler[Identity, Identity, U] = handlers.default[U](f(this))
*/