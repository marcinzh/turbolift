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

  val handler = new DefaultHandler

  class DefaultHandler extends Nullary[Option] {
    def lift[M[_] : MonadPar, A](ma: M[A]): M[Option[A]] = ma.map(Some(_))

    def flatMap[M[_] : MonadPar, A, B](tma: M[Option[A]])(f: A => M[Option[B]]): M[Option[B]] =
      tma.flatMap { 
        case Some(a) => f(a)
        case None => Monad[M].pure(None)
      }

    def zipPar[M[_] : MonadPar, A, B](tma: M[Option[A]], tmb: M[Option[B]]): M[Option[(A, B)]] =
      (tma *! tmb).map { 
        case (Some(a), Some(b)) => Some((a, b))
        case _ => None
      }

    def decode[M[+_] : MonadPar] = new Decode[M] with MaybeSig {
      val fail = Monad[M].pure(None)
    }
  }
}
