package turbolift.effects
import java.io.{Closeable => JCloseable}
import turbolift.{!!, Effect, Signature, Handler}
import turbolift.Extensions._
import turbolift.io.ResourceFactory


/** Polymorphic variant of [[ResourceEffect]].
 *
 * In the monomorphic variant, the `U` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * // `U` is explicitly set as `IO`:
 * case object MyResource extends ResourceEffect[IO]
 *
 * // `U` is inferred from the effect instance:
 * val computation = MyResource.register(IO(println("RESOURCE CLOSED!")))
 * }}}
 *
 * In the polymorphic variant, the `U` type parameter is **covariantly** inferred
 * at call sites of effect's operations and handlers:
 *
 * {{{
 * case object MyResource extends PolyResourceEffect
 *
 * // auxiliary definitions, because we need 2 distinct effects:
 * case object S1 extends StateEffect[Boolean]; type S1 = S1.type
 * case object S2 extends StateEffect[Boolean]; type S2 = S2.type
 *
 * val computation1 = MyResource.register(S1.put(false))  // `U` inferred as `S1`
 * val computation2 = MyResource.register(S2.put(false))  // `U` inferred as `S2`
 * val computation3 = computation1 &&! computation2       // `U` inferred as `S1 & S2`
 * }}}
 */
abstract class PolyResourceEffect extends Effect.Polymorphic_+[ResourceEffect, Any](new ResourceEffect[Any] {}):
  final def register[X](release: Unit !! X): Unit !! @@[X] = polymorphize[X].perform(_.register(release))
  final def use[X] = new UseApply[X]
  final def scoped[X] = new ScopedApply[X]

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class UseApply[X]:
    def apply[A](acquire: A !! X, release: A => Unit !! X): A !! @@[X] =
      polymorphize[X].perform(_.use(acquire, release))

    def apply(acquire: Unit !! X, release: Unit !! X): Unit !! @@[X] =
      polymorphize[X].perform(_.use(acquire, release))

    def apply[A](rf: ResourceFactory[A, X]): A !! @@[X] =
      polymorphize[X].perform(_.use(rf.acquire, rf.release(_)))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class ScopedApply[X]:
    def apply[A, U](comp: A !! (U & @@[X])): A !! (U & X) = comp.handleWith(handlers.default)


  /** Predefined handlers for this effect. */
  object handlers:
    def default[U]: Handler[Identity, Identity, @@[U], U] =
      polymorphize[U].handler(_.handlers.default)


/** Predefined instance of [[PolyResource]] effect.
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object PolyResource extends PolyResourceEffect:
  export handlers.{default => handler}
