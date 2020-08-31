package turbolift.std_handlers
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax
import turbolift.std_effects.{StateSig, State}


object DefaultStateHandler {
  def apply[S, Fx <: State[S]](fx: Fx) = new fx.Unary[S, (S, ?)] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](s: S, a: A): (S, A) = (s, a)

      def flatMap[A, B](tma: S => M[(S, A)])(f: A => S => M[(S, B)]): S => M[(S, B)] =
        s0 => tma(s0).flatMap {
          case (s1, a) => f(a)(s1)
        }

      def zipPar[A, B](tma: S => M[(S, A)], tmb: S => M[(S, B)]): S => M[(S, (A, B))] =
        s0 => tma(s0).flatMap {
          case (s1, a) => tmb(s1).map {
            case (s2, b) => (s2, (a, b))
          }
        }
    }

    def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with StateSig[U, S] {
      val get: S !! U =
        withLift { l => s =>
          pureInner((s, l.pureStash(s)))
        }

      def put(s: S): Unit !! U =
        withLift { l => _ =>
          pureInner((s, l.unitStash()))
        }
      
      override def mod(f: S => S): Unit !! U =
        withLift { l => s =>
          pureInner((f(s), l.unitStash()))
        }
    }
  }.self
}
