package turbolift.std_effects
// import cats.implicits._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax


trait StateSig[U, S] extends Signature[U] {
  def get: S !! U
  def put(s: S): Unit !! U
  def mod(f: S => S): Unit !! U = get.flatMap(s => put(f(s)))
}


trait State[S] extends Effect[StateSig[?, S]] {
  val get: S !! this.type = encodeFO(_.get)
  def put(s: S): Unit !! this.type = encodeFO(_.put(s))
  def mod(f: S => S): Unit !! this.type = encodeFO(_.mod(f))

  val handler = DefaultStateHandler[S, this.type](this)
}


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
