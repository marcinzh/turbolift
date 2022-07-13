package turbolift.typeclass
import cats.Functor


object FlippedPairFunctor:
  given [S]: Functor[(_, S)] with
    override def map[A, B](as: (A, S))(f: A => B): (B, S) = (f(as._1), as._2)
