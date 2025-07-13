package turbolift.effects
import turbolift.{!!, Effect, Handler}
import turbolift.Extensions._


/** Polymorphic variant of [[LazyMemoizerEffect]]. */
abstract class PolyLazyMemoizerEffect extends Effect.Polymorphic_-+[[X, Y] =>> LazyMemoizerEffect[X, Y], Any, Any](new LazyMemoizerEffect[Any, Any] {}):
  final def memo[K, V](k: K): (() => V) !! @@[K, V] = polymorphize[K, V].perform(_.memo(k))
  final def domain[K, V]: Set[K] !! @@[K, V] = polymorphize[K, V].perform(_.domain)
  final def toMap[K, V]: Map[K, V] !! @@[K, V] = polymorphize[K, V].perform(_.toMap)

  object handlers:
    def local[K, V, U](f: K => V !! (U & @@[K, V])): Handler[Identity, Identity, @@[K, V], U] =
      polymorphize[K, V]: p =>
        p.handler(_.handlers.local(k => p.lift(f(k))))

    def shared[K, V, U <: IO](f: K => V !! (U & @@[K, V])): Handler[Identity, Identity, @@[K, V], U] =
      polymorphize[K, V]: p =>
        p.handler(_.handlers.shared(k => p.lift(f(k))))


/** Predefined instance of [[PolyLazyMemoizerEffect]] effect.
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object PolyLazyMemoizer extends PolyLazyMemoizerEffect
