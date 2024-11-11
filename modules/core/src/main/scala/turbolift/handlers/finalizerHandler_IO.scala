package turbolift.handlers
import turbolift.!!
import turbolift.effects.{FinalizerEffect, FinalizerSignature, IO}
import turbolift.io.{Cause, Snap}
import turbolift.Extensions._


extension [U <: IO](fx: FinalizerEffect[U])
  def finalizerHandler_IO: fx.ThisHandler[Identity, Identity, U] =
    type S = Trail[U]

    new fx.impl.Stateful[Identity, (_, S), U] with fx.impl.Parallel.ForkJoin with FinalizerSignature[U]:
      override type Local = S

      override def onInitial: Local !! Any = !!.pure(Trail.End)

      override def onReturn(a: Unknown, s: Local): (Unknown, Local) !! Any = !!.pure((a, s))

      override def onRestart(a_s: (Unknown, Local)): Unknown !! ThisEffect =
        val (a, s) = a_s
        Local.modify(Trail.Then(s, _)).as(a)

      override def onUnknown(aa: (Unknown, Local)): Option[Unknown] = Some(aa._1)

      override def onZip[A, B, C](a_s: (A, Local), b_s: (B, Local), k: (A, B) => C): (C, Local) =
        val (a, s1) = a_s
        val (b, s2) = b_s
        (k(a, b), Trail.Both(s1, s2))

      override def onFork(s: Local): (Local, Local) = (s, Trail.End)

      override def use[A](acquire: A !! U, release: A => Unit !! U): A !! ThisEffect =
        IO.uncancellable:
          acquire.flatMap: a =>
            Local.modify(Trail.Then(Trail.One(release(a)), _)).as(a)

    .toHandler
    .tapStateEff: trail =>
      IO.uncancellable(trail.fold).flatMap(_.run)
    .dropState


private enum Trail[-U <: IO]:
  case End
  case One(action: Unit !! U)
  case Then(lhs: Trail[U], rhs: Trail[U])
  case Both(lhs: Trail[U], rhs: Trail[U])

  final def fold: Snap[Unit] !! U =
    this match
      case End => !!.pure(Snap.unit)
      case One(a) => IO.snap(a)
      case Then(a, b) => a.fold.zipWith(b.fold)(Trail.accumSeq)
      case Both(a, b) => a.fold.zipWithPar(b.fold)(Trail.accumPar)


private object Trail:
  val accumPar = lift(_ & _)
  val accumSeq = lift(_ ++ _)
  def lift(op: (Cause, Cause) => Cause)(a: Snap[Unit], b: Snap[Unit]): Snap[Unit] =
    ((a, b): @unchecked) match
      case (Snap.Failure(c), Snap.Failure(d)) => Snap.Failure(op(c, d))
      case (Snap.Failure(_), _) => a
      case (_, Snap.Failure(_)) => b
      case (Snap.Cancelled, _) => a
      case (_, Snap.Cancelled) => b
      case (Snap.Aborted(_, _), _) => a
      case (_, Snap.Aborted(_, _)) => b
      case _ => Snap.unit
