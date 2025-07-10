package turbolift.effects
import java.io.{Closeable => JCloseable}
import turbolift.{!!, Effect, Signature, Handler}
import turbolift.Extensions._
import turbolift.io.{ResourceFactory, Cause, Snap}
import turbolift.data.Trail


trait FinalizerSignature[U] extends Signature:
  def use[A](acquire: A !! U, release: A => Unit !! U): A !! ThisEffect


trait FinalizerEffect[U] extends Effect[FinalizerSignature[U]] with FinalizerSignature[U]:
  final override def use[A](acquire: A !! U, release: A => Unit !! U): A !! this.type = perform(_.use(acquire, release))

  final def use[A, U2 >: U](rf: ResourceFactory[A, U2]): A !! this.type =
    use(rf.acquire, rf.release(_))


object FinalizerEffect:
  extension [U <: IO](thiz: FinalizerEffect[U])
    def use[A <: JCloseable](acquire: => A): A !! thiz.type =
      thiz.use(IO(acquire), a => IO(a.close))

    def handlerIO: Handler[Identity, Identity, thiz.type, U] =
      new thiz.impl.Stateful[Identity, (_, Trail[U]), U] with thiz.impl.Parallel.ForkJoin with FinalizerSignature[U]:
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

        override def use[A](acquire: A !! U, release: A => Unit !! U): A !! ThisEffect =
          IO.uncancellable:
            acquire.flatMap: a =>
              Local.modify(Trail(release(a)) ++ _).as(a)

      .toHandler
      .tapStateEff: trail =>
        IO.uncancellable(trail.run)
      .dropState


trait FinalizerEffectIO[U <: IO] extends FinalizerEffect[U]:
  /** Default handler for this effect. */
  def handler = this.handlerIO

  def scoped[A, V](comp: A !! (V & this.type)): A !! (V & U) = comp.handleWith(handler)


/** Predefined instance of this effect. */
case object Finalizer extends FinalizerEffectIO[IO]

type Finalizer = Finalizer.type
