package turbolift.std_effects
import cats.instances.tuple._
import cats.syntax.functor._
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.{MonadPar, AccumZero}
import turbolift.abstraction.typeclass.Syntax._


object ReaderWriterStateHandler:
  object Syntax:
    extension [
      R, W, W1, S,
      FxR <: Reader[R],
      FxW <: WriterExt[W, W1],
      FxS <: State[S]
    ](thiz: Effect.Combine3[FxR, FxW, FxS])
      def handler(initialR: R, initialS: S)(implicit W: AccumZero[W, W1]): thiz.ThisIHandler[((W, S), _)] =
        ReaderWriterStateHandler.apply(thiz, initialR, initialS)

  def apply[
    R, W, W1, S,
    FxR <: Reader[R],
    FxW <: WriterExt[W, W1],
    FxS <: State[S]
  ](fx: Effect.Combine3[FxR, FxW, FxS], initialR: R, initialS: S)(implicit W: AccumZero[W, W1]): fx.ThisIHandler[((W, S), _)] =
    type RWS = (R, W, S)
    extension (thiz: RWS)
      inline def r: R = thiz._1
      inline def w: W = thiz._2
      inline def s: S = thiz._3
      inline def r_=(r2: R): RWS = (r2, w, s)
      inline def w_=(w2: W): RWS = (r, w2, s)
      inline def s_=(s2: S): RWS = (r, w, s2)
      inline def r_%(f: R => R): RWS = (f(r), w, s)
      inline def w_%(f: W => W): RWS = (r, f(w), s)
      inline def s_%(f: S => S): RWS = (r, w, f(s))
      inline def restore_r(that: RWS): RWS = (r, that.w, that.s)
      inline def combine_w(that: RWS, f: W => W): RWS = (r, w |+| f(that.w), that.s)


    new fx.Stateful[RWS, (RWS, _)]:
      override def onReturn[A](rws: RWS, a: A): (RWS, A) = (rws, a)

      override def onTransform[M[_]: MonadPar] = new Transformed[M]:
        override def flatMap[A, B](tma: RWS => M[(RWS, A)])(f: A => RWS => M[(RWS, B)]): RWS => M[(RWS, B)] =
          rws0 => tma(rws0).flatMap {
            case (rws1, a) => f(a)(rws0.restore_r(rws1))
          }

        override def zipPar[A, B](tma: RWS => M[(RWS, A)], tmb: RWS => M[(RWS, B)]): RWS => M[(RWS, (A, B))] =
          rws0 => tma(rws0).flatMap {
            case (rws1, a) => tmb(rws0.restore_r(rws1)).map {
              case (rws2, b) => (rws0.restore_r(rws2), (a, b))
            }
          }
      
      override def onOperation[M[_], F[_], U](implicit kk: ThisControl[M, F, U]) = new ReaderSig[U, R] with WriterExtSig[U, W, W1] with StateSig[U, S]:
        //////// Reader:

        override val ask: R !! U =
          kk.withLift(lift => rws => kk.pureInner((rws, lift.pureStash(rws.r))))

        override def asks[A](f: R => A): A !! U =
          kk.withLift(lift => rws => kk.pureInner((rws, lift.pureStash(f(rws.r)))))

        override def localPut[A](r: R)(body: A !! U): A !! U =
          kk.withLift(lift => rws => lift.run(body)(rws.r = r))

        override def localModify[A](f: R => R)(body: A !! U): A !! U =
          kk.withLift(lift => rws => lift.run(body)(rws.r_%(f)))

        //////// Writer:

        override def tell(w: W1): Unit !! U =
          kk.withLift(lift => rws => kk.pureInner((rws.w_%(_ |+ w), lift.unitStash())))

        override def tells(w: W): Unit !! U =
          kk.withLift(lift => rws => kk.pureInner((rws.w_%(_ |+| w), lift.unitStash())))

        override def listen[A](body: A !! U): (W, A) !! U =
          kk.withLift { lift => rws0 =>
            lift.run(body)(rws0.w = W.zero).map {
              case (rws1, fa) => (rws0.combine_w(rws1, w => w), fa.map((rws1.w, _)))
            }
          }

        override def censor[A](body: A !! U)(f: W => W): A !! U =
          kk.withLift { lift => rws0 =>
            lift.run(body)(rws0.w = W.zero).map {
              case (rws1, fa) => (rws0.combine_w(rws1, f), fa)
            }
          }

        override def mute[A](body: A !! U): A !! U = censor(body)(_ => W.zero)

        //////// State:

        override val get: S !! U =
          kk.withLift(lift => rws => kk.pureInner((rws, lift.pureStash(rws.s))))

        override def gets[A](f: S => A): A !! U =
          kk.withLift(lift => rws => kk.pureInner((rws, lift.pureStash(f(rws.s)))))

        override def put(s: S): Unit !! U =
          kk.withLift(lift => rws => kk.pureInner((rws.s = s, lift.unitStash())))

        override def swap(s: S): S !! U =
          kk.withLift(lift => rws => kk.pureInner((rws.s = s, lift.pureStash(rws.s))))

        override def modify(f: S => S): Unit !! U =
          kk.withLift(lift => rws => kk.pureInner((rws.s_%(f), lift.unitStash())))

        override def update[A](f: S => (S, A)): A !! U =
          kk.withLift { lift => rws =>
            val (s, a) = f(rws.s)
            kk.pureInner((rws.s = s, lift.pureStash(a)))
          }

    .toHandler((initialR, W.zero, initialS))
    .mapState { case (_, w, s) => (w, s) }

