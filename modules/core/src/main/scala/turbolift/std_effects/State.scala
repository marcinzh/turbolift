package turbolift.std_effects
import mwords._
// import turbolift.abstraction.!!
import turbolift.abstraction.effect._


trait StateSig[S] extends Signature {
  def get: Op[S]
  def put(s: S): Op[Unit]
}

trait State[S] extends Effect[StateSig[S]] with StateSig[S] {
  val get = encode(_.get)
  def put(s: S) = encode(_.put(s))
  def mod(f: S => S) = get.flatMap(s => put(f(s)))

  val handler = new DefaultHandler

  class DefaultHandler extends Unary[S, (S, +?)] {
    def commonOps[M[+_] : MonadPar] = new CommonOps[M] {
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

    def specialOps[M[+_] : MonadPar] = new SpecialOps[M] with StateSig[S] {
      val get = s => Monad[M].pure((s, s))
      def put(s: S) = _ => Monad[M].pure((s, ()))
    }
  }
}
