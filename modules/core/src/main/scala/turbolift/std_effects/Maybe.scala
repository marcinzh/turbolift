package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect._


trait MaybeSig extends FailSig


trait Maybe extends FilterableEffect[MaybeSig] with MaybeSig {
  
  def from[A](x: Option[A]): A !! this.type = x match {
    case Some(a) => pure(a)
    case _ => fail
  }

  val handler = MaybeHandler(this)
}


object MaybeHandler {
  def apply[Fx <: Maybe](effect: Fx) = new effect.Nullary[Option] {
    def commonOps[M[+_] : MonadPar] = new CommonOps[M] {
      def lift[A](ma: M[A]): M[Option[A]] = ma.map(Some(_))

      def flatMap[A, B](tma: M[Option[A]])(f: A => M[Option[B]]): M[Option[B]] =
        tma.flatMap {
          case Some(a) => f(a)
          case None => Monad[M].pure(None)
        }

      def zipPar[A, B](tma: M[Option[A]], tmb: M[Option[B]]): M[Option[(A, B)]] =
        (tma *! tmb).map {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }
    }

    def specialOps[M[+_] : MonadPar] = new SpecialOps[M] with MaybeSig {
      val fail = Monad[M].pure(None)
    }
  }.self
}
