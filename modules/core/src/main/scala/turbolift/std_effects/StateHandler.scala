package turbolift.std_effects
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object StateHandler:
  def apply[S, Fx <: State[S]](fx: Fx, initial: S): fx.ThisIHandler[(S, _)] =
    new fx.Stateful[S, (S, _)]:
      override def onReturn[A](s: S, a: A): (S, A) = (s, a)

      override def onTransform[M[_]: MonadPar] = new Transformed[M]:
        override def flatMap[A, B](tma: S => M[(S, A)])(f: A => S => M[(S, B)]): S => M[(S, B)] =
          s0 => tma(s0).flatMap {
            case (s1, a) => f(a)(s1)
          }

        override def zipPar[A, B](tma: S => M[(S, A)], tmb: S => M[(S, B)]): S => M[(S, (A, B))] =
          s0 => tma(s0).flatMap {
            case (s1, a) => tmb(s1).map {
              case (s2, b) => (s2, (a, b))
            }
          }
      
      override def onOperation[M[_], F[_], U](implicit kk: ThisControl[M, F, U]) = new StateSig[U, S]:
        override val get: S !! U =
          kk.withLift(lift => s => kk.pureInner((s, lift.pureStash(s))))

        override def gets[A](f: S => A): A !! U =
          kk.withLift(lift => s => kk.pureInner((s, lift.pureStash(f(s)))))

        override def put(s: S): Unit !! U =
          kk.withLift(lift => _ => kk.pureInner((s, lift.unitStash())))

        override def swap(s2: S): S !! U =
          kk.withLift(lift => s1 => kk.pureInner((s2, lift.pureStash(s1))))

        override def modify(f: S => S): Unit !! U =
          kk.withLift(lift => s => kk.pureInner((f(s), lift.unitStash())))

        override def update[A](f: S => (S, A)): A !! U =
          kk.withLift { lift => s1 =>
            val (s2, a) = f(s1)
            kk.pureInner((s2, lift.pureStash(a)))
          }
    
    .toHandler(initial)
