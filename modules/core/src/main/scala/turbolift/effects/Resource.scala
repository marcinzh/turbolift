package turbolift.effects
import java.io.{Closeable => JCloseable}
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.io.ResourceFactory
import turbolift.data.Trail


/** Signature of [[ResourceEffect]].
 *
 * @tparam U Type-level set of effects used to acquire/release the resources.
 */
trait ResourceSignature[U] extends Signature:
  def register(release: Unit !! U): Unit !! ThisEffect
  def use[A](acquire: A !! U, release: A => Unit !! U): A !! ThisEffect


/** Base trait for custom instances of Resource effect.
 *
 * {{{
 * case object MyResource extends ResourceEffect[java.io.InputStream]
 * // optional:
 * type MyResource = MyResource.type
 * }}}
 *
 * @see [[PolyResourceEffect]]
 * @see [[Resource]]
 *
 * @tparam U Type-level set of effects used to acquire/release the resources.
 */
trait ResourceEffect[U] extends Effect[ResourceSignature[U]] with ResourceSignature[U]:
  enclosing =>
  final override def register(release: Unit !! U): Unit !! this.type = perform(_.register(release))
  final override def use[A](acquire: A !! U, release: A => Unit !! U): A !! this.type = perform(_.use(acquire, release))

  final def use(acquire: Unit !! U, release: Unit !! U): Unit !! this.type = perform(_.use(acquire, _ => release))
  final def use[A](rf: ResourceFactory[A, U]): A !! this.type = use(rf.acquire, rf.release(_))
  final def scoped[A, V](comp: A !! (V & this.type)): A !! (V & U) = comp.handleWith(handlers.default)

  /** Predefined handlers for this effect. */
  object handlers:
    def default: Handler[Identity, Identity, enclosing.type, U] =
      new impl.Stateful[Identity, (_, Trail[U]), U] with impl.Parallel.ForkJoin with ResourceSignature[U]:
        override type Local = Trail[U]
        override def onInitial: Local !! Any = !!.pure(Trail.empty)
        override def onReturn(a: Unknown, s: Local): (Unknown, Local) !! Any = !!.pure((a, s))

        override def onRestart(a_s: (Unknown, Local)): Unknown !! ThisEffect =
          val (a, s) = a_s
          Local.modify(s ++ _).as(a)

        override def onUnknown(aa: (Unknown, Local)): Option[Unknown] = Some(aa._1)

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
      .tapStateEff: trail =>
        UnsafeIO.uncancellable(trail.run)
      .dropState


object ResourceEffect:
  extension [U](thiz: ResourceEffect[U])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler: Handler[Identity, Identity, thiz.type, U] = thiz.handlers.default


  extension [U <: IO](thiz: ResourceEffect[U])
    def register[A <: JCloseable](a: A): Unit !! thiz.type = register(IO(a.close))
    def use[A <: JCloseable](acquire: => A): A !! thiz.type = thiz.use(IO(acquire), a => IO(a.close))


/** Predefined instance of [[ResourceEffect]]. */
case object ResourceIO extends ResourceEffect[IO]
type ResourceIO = ResourceIO.type



/** Polymorphic variant of [[ResourceEffect]].
 *
 * In the monomorphic variant, the `U` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * // `The U` is explicitly set as `IO`:
 * case object MyResource extends ResourceEffect[IO]
 *
 * // The `U` is inferred from the effect instance:
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


object PolyResourceEffect:
  extension (thiz: PolyResourceEffect)
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler[U]: Handler[Identity, Identity, thiz.@@[U], U] = thiz.handlers.default


/** Predefined instance of [[PolyResourceEffect]].
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object Resource extends PolyResourceEffect
type Resource[U] = Resource.@@[U]
