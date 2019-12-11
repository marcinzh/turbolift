package turbolift.std_effects
import mwords._
// import turbolift.abstraction.!!
import turbolift.abstraction.effect._


trait NonDetSig extends FailSig {
  def each[A](as: Iterable[A]): Op[A]
}

trait NonDet extends FilterableEffect[NonDetSig] with NonDetSig {
  def each[A](as: Iterable[A]) = encode(_.each(as))
  
  def from[A](as: A*) = each(as.toVector)

  val handler = new DefaultHandler

  class DefaultHandler extends Nullary[Vector] {
    def commonOps[M[+_] : MonadPar] = new CommonOps[M] {
      def lift[A](ma: M[A]): M[Vector[A]] = ma.map(Vector(_))

      def flatMap[A, B](tma: M[Vector[A]])(f: A => M[Vector[B]]): M[Vector[B]] = {
        def loop(as: Vector[A]): M[Vector[B]] = as match {
          case Vector() => Monad[M].pure(Vector())
          case Vector(a) => f(a)
          case _ =>
            val (as1, as2) = as.splitAt(as.size / 2)
            (loop(as1) *! loop(as2)).map {
              case (bs1, bs2) => bs1 ++ bs2
            }
        }
        tma.flatMap(loop)
      }

      def zipPar[A, B](tma: M[Vector[A]], tmb: M[Vector[B]]): M[Vector[(A, B)]] =
        (tma *! tmb).map {
          case (as, bs) =>
            for {
              a <- as
              b <- bs
            } yield (a, b)
        }
    }

    def specialOps[M[+_] : MonadPar] = new SpecialOps[M] with NonDetSig {
      val fail = Monad[M].pure(Vector())
      def each[A](as: Iterable[A]) = Monad[M].pure(as.toVector)
    }
  }
}
