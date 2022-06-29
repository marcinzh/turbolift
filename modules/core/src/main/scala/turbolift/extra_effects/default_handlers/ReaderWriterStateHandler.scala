package turbolift.extra_effects.default_handlers
import cats.syntax.functor._
import turbolift.{!!, Effect}
import turbolift.typeclass.{MonadZip, AccumZero}
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{Reader, ReaderSig}
import turbolift.std_effects.{WriterEffect, WriterSig}
import turbolift.std_effects.{State, StateSig}
import turbolift.std_effects.default_handlers.FlippedPairFunctor.given


private[extra_effects] object ReaderWriterStateHandler:
  def apply[
    R, W, W1, S,
    FxR <: Reader[R],
    FxW <: WriterEffect[W, W1],
    FxS <: State[S]
  ](fx: Effect.Combine3[FxR, FxW, FxS], initialR: R, initialS: S)(implicit W: AccumZero[W, W1]): fx.ThisHandler.Free[(_, (W, S))] =
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


    new fx.Stateful[RWS, (_, RWS)] with ReaderSig[R] with WriterSig[W, W1] with StateSig[S]:
      override def onPure[A](a: A): RWS => (A, RWS) = (a, _)

      override def onFlatMap[A, B, M[_]: MonadZip](tma: RWS => M[(A, RWS)])(f: A => RWS => M[(B, RWS)]): RWS => M[(B, RWS)] =
        rws0 => tma(rws0).flatMap {
          case (a, rws1) => f(a)(rws0.restore_r(rws1))
        }

      override def onZip[A, B, M[_]: MonadZip](tma: RWS => M[(A, RWS)], tmb: RWS => M[(B, RWS)]): RWS => M[((A, B), RWS)] =
        rws0 => tma(rws0).flatMap {
          case (a, rws1) => tmb(rws0.restore_r(rws1)).map {
            case (b, rws2) => ((a, b), rws0.restore_r(rws2))
          }
        }
      
      //////// Reader:

      override val ask: R !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(rws.r), rws))

      override def asks[A](f: R => A): A !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(f(rws.r)), rws))

      override def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !@! U =
        kk ?=> rws => kk.locally(body)(rws.r = r)

      override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !@! U =
        kk ?=> rws => kk.locally(body)(rws.r_%(f))

      //////// Writer:

      override def tell(w: W1): Unit !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(), rws.w_%(_ |+ w)))

      override def tells(w: W): Unit !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(), rws.w_%(_ |+| w)))

      override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !@! U =
        kk ?=> rws0 => kk.locally(body)(rws0.w = W.zero).map {
          case (fa, rws1) => (fa.map((_, rws1.w)), rws0.combine_w(rws1, w => w))
        }

      override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !@! U =
        kk ?=> rws0 => kk.locally(body)(rws0.w = W.zero).map {
          case (fa, rws1) => (fa, rws0.combine_w(rws1, f))
        }

      override def mute[A, U <: ThisEffect](body: A !! U): A !@! U =
        kk ?=> rws0 => censor(_ => W.zero)(body)(rws0)

      //////// State:

      override val get: S !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(rws.s), rws))

      override def gets[A](f: S => A): A !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(f(rws.s)), rws))

      override def put(s: S): Unit !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(), rws.s = s))

      override def swap(s: S): S !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(rws.s), rws.s = s))

      override def modify(f: S => S): Unit !@! ThisEffect =
        kk ?=> rws => kk.outer((kk.inner(), rws.s_%(f)))

      override def modifyGet(f: S => S): S !@! ThisEffect =
        kk ?=> rws =>
          val s = f(rws.s)
          kk.outer((kk.inner(s), rws.s = s))

      override def getModify(f: S => S): S !@! ThisEffect =
        kk ?=> rws =>
          val s = f(rws.s)
          kk.outer((kk.inner(rws.s), rws.s = s))

      override def getModifyGet(f: S => S): (S, S) !@! ThisEffect =
        kk ?=> rws =>
          val s = f(rws.s)
          kk.outer((kk.inner((s, rws.s)), rws.s = s))

      override def update[A](f: S => (A, S)): A !@! ThisEffect =
        kk ?=> rws =>
          val (a, s) = f(rws.s)
          kk.outer((kk.inner(a), rws.s = s))

      override def updateGet[A](f: S => (A, S)): (A, S) !@! ThisEffect =
        kk ?=> rws =>
          val a_s @ (_, s) = f(rws.s)
          kk.outer((kk.inner(a_s), rws.s = s))

      override def getUpdate[A](f: S => (A, S)): (A, S) !@! ThisEffect =
        kk ?=> rws =>
          val (a, s) = f(rws.s)
          kk.outer((kk.inner((a, rws.s)), rws.s = s))

      override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !@! ThisEffect =
        kk ?=> rws =>
          val (a, s) = f(rws.s)
          kk.outer((kk.inner((a, rws.s, s)), rws.s = s))

    .toHandler((initialR, W.zero, initialS))
    .mapState { case (_, w, s) => (w, s) }

