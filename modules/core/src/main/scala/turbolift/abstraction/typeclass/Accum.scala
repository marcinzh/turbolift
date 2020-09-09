package turbolift.abstraction.typeclass
import cats.{Semigroup, SemigroupK, Applicative}


trait Accum[W, WW] {
  def one(e: W): WW
  def plus(x: WW, y: WW): WW
  def plus1(x: WW, e: W): WW // = plus(x, one(e))
}

object Accum extends AccumInstances2 {
  def apply[W, WW](implicit ev: Accum[W, WW]) = ev
}

trait AccumInstances1 {
  implicit def fromSemigroup[W](implicit W: Semigroup[W]): Accum[W, W] =
    new Accum[W, W] {
      override def one(w: W): W = w
      override def plus(w1: W, w2: W): W = W.combine(w1, w2)
      override def plus1(w1: W, w2: W): W = W.combine(w1, w2)
    }

  implicit def fromSemigroupK[W, F[_]](implicit W: SemigroupK[F], Appl: Applicative[F]): Accum[W, F[W]] =
    new Accum[W, F[W]] {
      override def one(w: W): F[W] = Appl.pure(w)
      override def plus(fw1: F[W], fw2: F[W]): F[W] = W.combineK(fw1, fw2)
      override def plus1(fw: F[W], w: W): F[W] = W.combineK(fw, one(w))
    }
}

trait AccumInstances2 extends AccumInstances1 {
  implicit def forVector[W] = AccumZero.forVector[W].toAccum
  implicit def forList[W] = AccumZero.forList[W].toAccum
  implicit def forSet[W] = AccumZero.forSet[W].toAccum
  implicit def forArray[W: reflect.ClassTag] = AccumZero.forArray[W].toAccum
}

trait AccumImplicits {
  implicit class AccumSyntax[W, WW](x: WW)(implicit WW: Accum[W, WW]) {
    def |+|(y: WW): WW = WW.plus(x, y)
    def |+(e: W): WW = WW.plus1(x, e)
  }
}
