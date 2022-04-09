package turbolift.extra_effects.default_handlers
import cats.instances.tuple._
import cats.syntax.functor._
import turbolift.{!!, Effect}
import turbolift.typeclass.{MonadPar, AccumZero}
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{Reader, ReaderSig}
import turbolift.std_effects.{WriterExt, WriterExtSig}
import turbolift.std_effects.{State, StateSig}


private[extra_effects] object ReaderWriterStateHandler:
  def apply[
    R, W, W1, S,
    FxR <: Reader[R],
    FxW <: WriterExt[W, W1],
    FxS <: State[S]
  ](fx: Effect.Combine3[FxR, FxW, FxS], initialR: R, initialS: S)(implicit W: AccumZero[W, W1]): fx.ThisHandler.Free[((W, S), _)] =
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


    new fx.Stateful[RWS, (RWS, _)] with ReaderSig[R] with WriterExtSig[W, W1] with StateSig[S]:
      override def onReturn[A](a: A): RWS => (RWS, A) = (_, a)

      override def onFlatMap[A, B, M[_]: MonadPar](tma: RWS => M[(RWS, A)])(f: A => RWS => M[(RWS, B)]): RWS => M[(RWS, B)] =
        rws0 => tma(rws0).flatMap {
          case (rws1, a) => f(a)(rws0.restore_r(rws1))
        }

      override def onProduct[A, B, M[_]: MonadPar](tma: RWS => M[(RWS, A)], tmb: RWS => M[(RWS, B)]): RWS => M[(RWS, (A, B))] =
        rws0 => tma(rws0).flatMap {
          case (rws1, a) => tmb(rws0.restore_r(rws1)).map {
            case (rws2, b) => (rws0.restore_r(rws2), (a, b))
          }
        }
      
      //////// Reader:

      override val ask: R !@! ThisEffect =
        kk ?=> rws => kk.outer((rws, kk.inner(rws.r)))

      override def asks[A](f: R => A): A !@! ThisEffect =
        kk ?=> rws => kk.outer((rws, kk.inner(f(rws.r))))

      override def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !@! U =
        kk ?=> rws => kk.locally(body)(rws.r = r)

      override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !@! U =
        kk ?=> rws => kk.locally(body)(rws.r_%(f))

      //////// Writer:

      override def tell(w: W1): Unit !@! ThisEffect =
        kk ?=> rws => kk.outer((rws.w_%(_ |+ w), kk.inner()))

      override def tells(w: W): Unit !@! ThisEffect =
        kk ?=> rws => kk.outer((rws.w_%(_ |+| w), kk.inner()))

      override def listen[A, U <: ThisEffect](body: A !! U): (W, A) !@! U =
        kk ?=> rws0 => kk.locally(body)(rws0.w = W.zero).map {
          case (rws1, fa) => (rws0.combine_w(rws1, w => w), fa.map((rws1.w, _)))
        }

      override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !@! U =
        kk ?=> rws0 => kk.locally(body)(rws0.w = W.zero).map {
          case (rws1, fa) => (rws0.combine_w(rws1, f), fa)
        }

      override def mute[A, U <: ThisEffect](body: A !! U): A !@! U =
        kk ?=> rws0 => censor(_ => W.zero)(body)(rws0)

      //////// State:

      override val get: S !@! ThisEffect =
        kk ?=> rws => kk.outer((rws, kk.inner(rws.s)))

      override def gets[A](f: S => A): A !@! ThisEffect =
        kk ?=> rws => kk.outer((rws, kk.inner(f(rws.s))))

      override def put(s: S): Unit !@! ThisEffect =
        kk ?=> rws => kk.outer((rws.s = s, kk.inner()))

      override def swap(s: S): S !@! ThisEffect =
        kk ?=> rws => kk.outer((rws.s = s, kk.inner(rws.s)))

      override def modify(f: S => S): Unit !@! ThisEffect =
        kk ?=> rws => kk.outer((rws.s_%(f), kk.inner()))

      override def modifyGet(f: S => S): S !@! ThisEffect =
        kk ?=> rws =>
          val s = f(rws.s)
          kk.outer((rws.s = s, kk.inner(s)))

      override def getModify(f: S => S): S !@! ThisEffect =
        kk ?=> rws =>
          val s = f(rws.s)
          kk.outer((rws.s = s, kk.inner(rws.s)))

      override def getModifyGet(f: S => S): (S, S) !@! ThisEffect =
        kk ?=> rws =>
          val s = f(rws.s)
          kk.outer((rws.s = s, kk.inner((rws.s, s))))

      override def update[A](f: S => (S, A)): A !@! ThisEffect =
        kk ?=> rws =>
          val (s, a) = f(rws.s)
          kk.outer((rws.s = s, kk.inner(a)))

      override def updateGet[A](f: S => (S, A)): (S, A) !@! ThisEffect =
        kk ?=> rws =>
          val sa @ (s, _) = f(rws.s)
          kk.outer((rws.s = s, kk.inner(sa)))

      override def getUpdate[A](f: S => (S, A)): (S, A) !@! ThisEffect =
        kk ?=> rws =>
          val (s, a) = f(rws.s)
          kk.outer((rws.s = s, kk.inner((rws.s, a))))

      override def getUpdateGet[A](f: S => (S, A)): (S, S, A) !@! ThisEffect =
        kk ?=> rws =>
          val (s, a) = f(rws.s)
          kk.outer((rws.s = s, kk.inner((rws.s, s, a))))

    .toHandler((initialR, W.zero, initialS))
    .mapState { case (_, w, s) => (w, s) }

