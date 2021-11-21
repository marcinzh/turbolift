package turbolift.std_effects
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object StateHandler:
  def apply[S, Fx <: State[S]](fx: Fx, initial: S): fx.ThisIHandler[(S, _)] =
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

      override def update[A](f: S => (S, A)): A !@! ThisEffect =
        kk ?=> s1 =>
          val (s2, a) = f(s1)
          kk.outer((s2, kk.inner(a)))
    
    .toHandler(initial)
