package turbolift.std_effects
// import cats.implicits._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax


trait StateSig[P[_], S] extends Signature[P] {
  def get: P[S]
  def put(s: S): P[Unit]
  def mod(f: S => S): P[Unit] = get.flatMap(s => put(f(s)))
}


trait State[S] extends Effect[StateSig[?[_], S]] {
  val get: S !! this.type = encodeFO(_.get)
  def put(s: S): Unit !! this.type = encodeFO(_.put(s))
  def mod(f: S => S): Unit !! this.type = encodeFO(_.mod(f))

  val handler = DefaultStateHandler[S, this.type](this)
}


object DefaultStateHandler {
  def apply[S, Fx <: State[S]](fx: Fx) = new fx.Unary[S, (S, ?)] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](s: S, a: A): (S, A) = (s, a)

      def lift[A](ma: M[A]): S => M[(S, A)] = s => ma.map((s, _))

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

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with StateSig[P, S] {
      val get: P[S] =
        withLift { l => s =>
          pureInner((s, l.pureStash(s)))
        }

      def put(s: S): P[Unit] =
        withLift { l => _ =>
          pureInner((s, l.unitStash()))
        }
      
      override def mod(f: S => S): P[Unit] =
        withLift { l => s =>
          pureInner((f(s), l.unitStash()))
        }
    }
  }.self
}
