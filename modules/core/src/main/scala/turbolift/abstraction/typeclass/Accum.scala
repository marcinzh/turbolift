package turbolift.abstraction.typeclass
import cats.{Semigroup, SemigroupK, Applicative}


trait Accum[W, W1] {
  def one(e: W1): W
  def plus(x: W, y: W): W
  def plus1(x: W, e: W1): W // = plus(x, one(e))
}

object Accum extends AccumInstances2 {
  def apply[W, W1](implicit ev: Accum[W, W1]) = ev
}

trait AccumInstances1 {
  implicit def fromSemigroup[W](implicit W: Semigroup[W]): Accum[W, W] =
    new Accum[W, W] {
      override def one(a: W): W = a
      override def plus(a: W, b: W): W = W.combine(a, b)
      override def plus1(a: W, b: W): W = W.combine(a, b)
    }

  implicit def fromSemigroupK[W, F[_]](implicit W: SemigroupK[F], Appl: Applicative[F]): Accum[F[W], W] =
    new Accum[F[W], W] {
      override def one(a: W): F[W] = Appl.pure(a)
      override def plus(a: F[W], b: F[W]): F[W] = W.combineK(a, b)
      override def plus1(a: F[W], b: W): F[W] = W.combineK(a, one(b))
    }
}

trait AccumInstances2 extends AccumInstances1 {
  implicit def forVector[W] = AccumZero.forVector[W].toAccum
  implicit def forList[W] = AccumZero.forList[W].toAccum
  implicit def forSet[W] = AccumZero.forSet[W].toAccum
  implicit def forArray[W: reflect.ClassTag] = AccumZero.forArray[W].toAccum
  implicit def forMap[K, V, V1](implicit V: Accum[V, V1]) = AccumZero.forMap[K, V, V1].toAccum
}

trait AccumImplicits {
  implicit class AccumSyntax[W, W1](thiz: W)(implicit W: Accum[W, W1]) {
    def |+|(that: W): W = W.plus(thiz, that)
    def |+(that: W1): W = W.plus1(thiz, that)
  }
}
