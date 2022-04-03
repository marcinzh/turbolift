package turbolift.std_effects.default_handlers
import cats.instances.tuple._
import turbolift.!!
import turbolift.typeclass.MonadPar
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{State, StateSig}


private[std_effects] object StateHandler:
  def apply[S, Fx <: State[S]](fx: Fx, initial: S): fx.ThisHandler.Free[(S, _)] =
    new fx.Stateful[S, (S, _)] with StateSig[S]:
      override def onReturn[A](a: A): S => (S, A) = (_, a)

      override def onFlatMap[A, B, M[_]: MonadPar](tma: S => M[(S, A)])(f: A => S => M[(S, B)]): S => M[(S, B)] =
        s0 => tma(s0).flatMap {
          case (s1, a) => f(a)(s1)
        }

      override def onProduct[A, B, M[_]: MonadPar](tma: S => M[(S, A)], tmb: S => M[(S, B)]): S => M[(S, (A, B))] =
        s0 => tma(s0).flatMap {
          case (s1, a) => tmb(s1).map {
            case (s2, b) => (s2, (a, b))
          }
        }
      
      override val get: S !@! ThisEffect =
        kk ?=> s => kk.outer((s, kk.inner(s)))

      override def gets[A](f: S => A): A !@! ThisEffect =
        kk ?=> s => kk.outer((s, kk.inner(f(s))))

      override def put(s: S): Unit !@! ThisEffect =
        kk ?=> _ => kk.outer((s, kk.inner()))

      override def swap(s2: S): S !@! ThisEffect =
        kk ?=> s1 => kk.outer((s2, kk.inner(s1)))

      override def modify(f: S => S): Unit !@! ThisEffect =
        kk ?=> s => kk.outer((f(s), kk.inner()))

      override def modifyGet(f: S => S): S !@! ThisEffect =
        kk ?=> s1 =>
          val s2 = f(s1)
          kk.outer((s2, kk.inner(s2)))

      override def getModify(f: S => S): S !@! ThisEffect =
        kk ?=> s1 =>
          val s2 = f(s1)
          kk.outer((s2, kk.inner(s1)))

      override def getModifyGet(f: S => S): (S, S) !@! ThisEffect =
        kk ?=> s1 =>
          val s2 = f(s1)
          kk.outer((s2, kk.inner((s1, s2))))

      override def update[A](f: S => (S, A)): A !@! ThisEffect =
        kk ?=> s1 =>
          val (s2, a) = f(s1)
          kk.outer((s2, kk.inner(a)))

      override def updateGet[A](f: S => (S, A)): (S, A) !@! ThisEffect =
        kk ?=> s1 =>
          val sa @ (s2, _) = f(s1)
          kk.outer((s2, kk.inner(sa)))

      override def getUpdate[A](f: S => (S, A)): (S, A) !@! ThisEffect =
        kk ?=> s1 =>
          val (s2, a) = f(s1)
          kk.outer((s2, kk.inner((s1, a))))

      override def getUpdateGet[A](f: S => (S, A)): (S, S, A) !@! ThisEffect =
        kk ?=> s1 =>
          val (s2, a) = f(s1)
          kk.outer((s2, kk.inner((s1, s2, a))))


    .toHandler(initial)
