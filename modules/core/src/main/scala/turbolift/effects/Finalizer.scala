package turbolift.effects
import java.io.{Closeable => JCloseable}
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.data.{Resource, Trail}


/** Signature of [[FinalizerEffect]].
 *
 * @tparam U Type-level set of effects used to acquire/release the resources.
 */
trait FinalizerSignature[U] extends Signature:
  def register(release: Unit !! U): Unit !! ThisEffect
  def use[A](acquire: A !! U, release: A => Unit !! U): A !! ThisEffect


/** Base trait for custom instances of Finalizer effect.
 *
 * {{{
 * case object MyFinalizer extends FinalizerEffect[IO]
 * // optional:
 * type MyFinalizer = MyFinalizer.type
 * }}}
 *
 * @see [[PolyFinalizerEffect]]
 * @see [[Finalizer]]
 *
 * @tparam U Type-level set of effects used to acquire/release the resources.
 */
trait FinalizerEffect[U] extends Effect[FinalizerSignature[U]] with FinalizerSignature[U]:
  enclosing =>
  final override def register(release: Unit !! U): Unit !! this.type = perform(_.register(release))
  final override def use[A](acquire: A !! U, release: A => Unit !! U): A !! this.type = perform(_.use(acquire, release))

  final def use(acquire: Unit !! U, release: Unit !! U): Unit !! this.type = perform(_.use(acquire, _ => release))
  final def use[A](r: Resource[A, U]): A !! this.type = use(r.acquire, r.release(_))
  final def scoped[A, V](comp: A !! (V & this.type)): A !! (V & U) = comp.handleWith(handlers.default)

  /** Predefined handlers for this effect. */
  object handlers:
    def default[U]: Handler.IdId[enclosing.type, U] =
      Handler.fromFunction([A, V] => (comp: A !! (V & enclosing.type)) => {
        underlying[U].handle(comp)
        .onSuccess((_, trail) => trail.run)
        .map(_._1)
      })

    private def underlying[U]: Handler[Identity, (_, Trail[U]), enclosing.type, U] =
      new impl.Stateful[Identity, (_, Trail[U]), U] with impl.Parallel.ForkJoin with FinalizerSignature[U]:
        override type Local = Trail[U]
        override def onInitial: Local !! Any = !!.pure(Trail.empty)
        override def onReturn(a: Unknown, s: Local): (Unknown, Local) !! Any = !!.pure((a, s))
        override def onAbort(s: Local): Unit !! ThisEffect = s.run

        override def onRestart(a_s: (Unknown, Local)): Unknown !! ThisEffect =
          val (a, s) = a_s
          Local.modify(s ++ _).as(a)

        override def onOnce(aa: (Unknown, Local)): Option[Unknown] = Some(aa._1)

        override def onZip[A, B, C](a_s: (A, Local), b_s: (B, Local), k: (A, B) => C): (C, Local) =
          val (a, s1) = a_s
          val (b, s2) = b_s
          (k(a, b), s1 & s2)

        override def onFork(s: Local): (Local, Local) = (s, Trail.empty)

        override def register(release: Unit !! U): Unit !! ThisEffect = Local.modify(Trail(release) ++ _)

        override def use[A](acquire: A !! U, release: A => Unit !! U): A !! ThisEffect =
          UnsafeIO.uncancellable:
            acquire.flatMap: a =>
              Local.modify(Trail(release(a)) ++ _).as(a)

      .toHandler


object FinalizerEffect:
  extension [U](thiz: FinalizerEffect[U])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler: Handler[Identity, Identity, thiz.type, U] = thiz.handlers.default


  extension [U <: IO](thiz: FinalizerEffect[U])
    def register[A <: JCloseable](a: A): Unit !! thiz.type = register(IO(a.close))
    def use[A <: JCloseable](acquire: A !! U): A !! thiz.type = thiz.use(acquire, a => IO(a.close))


/** Predefined instance of [[FinalizerEffect]]. */
case object FinalizerIO extends FinalizerEffect[IO]
type FinalizerIO = FinalizerIO.type



/** Polymorphic variant of [[FinalizerEffect]].
 *
 * In the monomorphic variant, the `U` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * // `The U` is explicitly set as `IO`:
 * case object MyFinalizer extends FinalizerEffect[IO]
 *
 * // The `U` is inferred from the effect instance:
 * val computation = MyFinalizer.register(IO(println("RESOURCE CLOSED!")))
 * }}}
 *
 * In the polymorphic variant, the `U` type parameter is **covariantly** inferred
 * at call sites of effect's operations and handlers:
 *
 * {{{
 * case object MyFinalizer extends PolyFinalizerEffect
 *
 * // auxiliary definitions, because we need 2 distinct effects:
 * case object S1 extends StateEffect[Boolean]; type S1 = S1.type
 * case object S2 extends StateEffect[Boolean]; type S2 = S2.type
 *
 * val computation1 = MyFinalizer.register(S1.put(false))  // `U` inferred as `S1`
 * val computation2 = MyFinalizer.register(S2.put(false))  // `U` inferred as `S2`
 * val computation3 = computation1 &&! computation2        // `U` inferred as `S1 & S2`
 * }}}
 */
abstract class PolyFinalizerEffect extends Effect.Polymorphic_+[FinalizerEffect, Any](new FinalizerEffect[Any] {}):
  final def register[X](release: Unit !! X): Unit !! @@[X] = polymorphize[X].perform(_.register(release))
  final def register[A <: JCloseable](a: A): Unit !! @@[IO] = polymorphize[IO].perform(_.register(a))
  final def use[X] = new UseApply[X]
  final def scoped[X] = new ScopedApply[X]

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class UseApply[X]:
    def apply[A](acquire: A !! X, release: A => Unit !! X): A !! @@[X] =
      polymorphize[X].perform(_.use(acquire, release))

    def apply(acquire: Unit !! X, release: Unit !! X): Unit !! @@[X] =
      polymorphize[X].perform(_.use(acquire, release))

    def apply[A <: JCloseable, X2 <: X & IO](acquire: A !! X2): A !! @@[X2] =
      polymorphize[X2].perform(_.use(acquire))

    def apply[A](r: Resource[A, X]): A !! @@[X] =
      polymorphize[X].perform(_.use(r))


  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class ScopedApply[X]:
    def apply[A, U](comp: A !! (U & @@[X])): A !! (U & X) = comp.handleWith(handlers.default)


  /** Predefined handlers for this effect. */
  object handlers:
    def default[U]: Handler[Identity, Identity, @@[U], U] =
      polymorphize[U].handler(_.handlers.default)


object PolyFinalizerEffect:
  extension (thiz: PolyFinalizerEffect)
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler[U]: Handler[Identity, Identity, thiz.@@[U], U] = thiz.handlers.default


/** Predefined instance of [[PolyFinalizerEffect]].
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object Finalizer extends PolyFinalizerEffect
type Finalizer[U] = Finalizer.@@[U]
