package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.Extensions._
import turbolift.handlers.{lazyMemoizerHandler_local, lazyMemoizerHandler_shared}


/** Memoizes a recursive, effectful, lazy function.
 *
 *  Like the `Memoizer` effect, but this version can be used to build or transform
 *  **cyclic** data structures.
 *
 *  The price to pay is that that `memo` returns a thunk.
 *  The constructor of the cyclic data structure should store obtained thunks,
 *  rather than attempt to call them.
 *
 *  Calling obtained thunks *BEFORE* this effect is handled,
 *  may raise `TieTheKnot` exception and should be considered a defect.
 */

trait LazyMemoizerSignature[K, V] extends Signature:
  def memo(k: K): (() => V) !! ThisEffect
  def domain: Set[K] !! ThisEffect
  def toMap: Map[K, V] !! ThisEffect


trait LazyMemoizerEffect[K, V] extends Effect[LazyMemoizerSignature[K, V]] with LazyMemoizerSignature[K, V]:
  enclosing =>
  final override def memo(k: K): (() => V) !! this.type = perform(_.memo(k))
  final override def domain: Set[K] !! this.type = perform(_.domain)
  final override def toMap: Map[K, V] !! this.type = perform(_.toMap)

  final def apply(k: K): (() => V) !! this.type = memo(k)

  /** Predefined handlers for this effect. */
  object handlers:
    def local[U](f: K => V !! (U & enclosing.type)): ThisHandler[Identity, Identity, U] = enclosing.lazyMemoizerHandler_local[U](f)
    def shared[U <: IO](f: K => V !! (U & enclosing.type)): ThisHandler[Identity, Identity, U] = enclosing.lazyMemoizerHandler_shared[U](f)


trait LazyMemoizer[K, V] extends LazyMemoizerEffect[K, V]:
  export handlers.{local => handler}


//@#@TODO `fix` syntax doesn't work in Scala 3.6.3
/*
object LazyMemoizer:
  trait Fix[K, V, U] extends LazyMemoizerEffect[K, V]:
    val handler: ThisHandler[Identity, Identity, U]

  def fix[K, V, U](f: (fx: Fix[K, V, U]) => K => V !! (U & fx.type)): Fix[K, V, U] = new:
    override val handler: ThisHandler[Identity, Identity, U] = handlers.default[U](f(this))
*/