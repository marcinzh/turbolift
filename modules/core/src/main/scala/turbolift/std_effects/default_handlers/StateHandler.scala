package turbolift.std_effects.default_handlers
import turbolift.!!
import turbolift.typeclass.MonadZip
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{State, StateSig}
import FlippedPairFunctor.given


private[std_effects] object StateHandler:
  def apply[S, Fx <: State[S]](fx: Fx, initial: S): fx.ThisHandler.Free[(_, S)] =
    new fx.Stateful[S, (_, S)] with StateSig[S]:
      override def onPure[A](a: A): S => (A, S) = (a, _)

      override def onFlatMap[A, B, M[_]: MonadZip](tma: S => M[(A, S)])(f: A => S => M[(B, S)]): S => M[(B, S)] =
        s0 => tma(s0).flatMap {
          case (a, s1) => f(a)(s1)
        }

      override def onZip[A, B, M[_]: MonadZip](tma: S => M[(A, S)], tmb: S => M[(B, S)]): S => M[((A, B), S)] =
        s0 => tma(s0).flatMap {
          case (a, s1) => tmb(s1).map {
            case (b, s2) => ((a, b), s2)
          }
        }
      
      override val get: S !@! ThisEffect =
        kk ?=> s => kk.outer((kk.inner(s), s))

      override def gets[A](f: S => A): A !@! ThisEffect =
        kk ?=> s => kk.outer((kk.inner(f(s)), s))

      override def put(s: S): Unit !@! ThisEffect =
        kk ?=> _ => kk.outer((kk.inner(), s))

      override def swap(s2: S): S !@! ThisEffect =
        kk ?=> s1 => kk.outer((kk.inner(s1), s2))

      override def modify(f: S => S): Unit !@! ThisEffect =
        kk ?=> s => kk.outer((kk.inner(), f(s)))

      override def modifyGet(f: S => S): S !@! ThisEffect =
        kk ?=> s1 =>
          val s2 = f(s1)
          kk.outer((kk.inner(s2), s2))

      override def getModify(f: S => S): S !@! ThisEffect =
        kk ?=> s1 =>
          val s2 = f(s1)
          kk.outer((kk.inner(s1), s2))

      override def getModifyGet(f: S => S): (S, S) !@! ThisEffect =
        kk ?=> s1 =>
          val s2 = f(s1)
          kk.outer((kk.inner((s1, s2)), s2))

      override def update[A](f: S => (A, S)): A !@! ThisEffect =
        kk ?=> s1 =>
          val (a, s2) = f(s1)
          kk.outer((kk.inner(a), s2))

      override def updateGet[A](f: S => (A, S)): (A, S) !@! ThisEffect =
        kk ?=> s1 =>
          val as @ (_, s2) = f(s1)
          kk.outer((kk.inner(as), s2))

      override def getUpdate[A](f: S => (A, S)): (A, S) !@! ThisEffect =
        kk ?=> s1 =>
          val (a, s2) = f(s1)
          kk.outer((kk.inner((a, s1)), s2))

      override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !@! ThisEffect =
        kk ?=> s1 =>
          val (a, s2) = f(s1)
          kk.outer((kk.inner((a, s1, s2)), s2))


    .toHandler(initial)
