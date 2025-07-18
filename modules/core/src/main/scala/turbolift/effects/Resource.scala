package turbolift.effects
import java.io.{Closeable => JCloseable}
import turbolift.{!!, Effect, Signature, Handler}
import turbolift.Extensions._
import turbolift.io.ResourceFactory
import turbolift.data.Trail


trait ResourceSignature[U] extends Signature:
  def register(release: Unit !! U): Unit !! ThisEffect
  def use[A](acquire: A !! U, release: A => Unit !! U): A !! ThisEffect


trait ResourceEffect[U] extends Effect[ResourceSignature[U]] with ResourceSignature[U]:
  enclosing =>
  final override def register(release: Unit !! U): Unit !! this.type = perform(_.register(release))
  final override def use[A](acquire: A !! U, release: A => Unit !! U): A !! this.type = perform(_.use(acquire, release))

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
  extension [U <: IO](thiz: ResourceEffect[U])
    def register[A <: JCloseable](a: A): Unit !! thiz.type = register(IO(a.close))
    def use[A <: JCloseable](acquire: => A): A !! thiz.type = thiz.use(IO(acquire), a => IO(a.close))


/** Predefined instance of [[Resource]] effect. */
case object Resource extends ResourceEffect[IO]:
  export handlers.{default => handler}

type Resource = Resource.type
