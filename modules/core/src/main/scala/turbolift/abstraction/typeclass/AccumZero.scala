package turbolift.abstraction.typeclass
import scala.collection.IterableFactory
import cats.{Monoid, MonoidK, Alternative}


trait AccumZero[W, WW] extends Accum[W, WW] {
  def zero: WW
  def toAccum: Accum[W, WW] = this
}

object AccumZero extends AccumZeroInstances2 {
  def apply[W, WW](implicit ev: AccumZero[W, WW]) = ev
}

trait AccumZeroInstances1 {
  implicit def fromMonoid[W](implicit W: Monoid[W]): AccumZero[W, W] =
    new AccumZero[W, W] {
      override def zero: W = W.empty
      override def one(w: W): W = w
      override def plus(w1: W, w2: W): W = W.combine(w1, w2)
      override def plus1(w1: W, w2: W): W = W.combine(w1, w2)
    }

  implicit def fromMonoidK[W, F[_]](implicit W: Alternative[F]): AccumZero[W, F[W]] =
    new AccumZero[W, F[W]] {
      override def zero: F[W] = W.empty
      override def one(w: W): F[W] = W.pure(w)
      override def plus(fw1: F[W], fw2: F[W]): F[W] = W.combineK(fw1, fw2)
      override def plus1(fw: F[W], w: W): F[W] = W.combineK(fw, one(w))
    }
}

trait AccumZeroInstances2 extends AccumZeroInstances1 {
  implicit def forVector[W] = make[W, Vector](Vector, Vector(_), _ :+ _)
  implicit def forList[W] = make[W, List](List, List(_), _ :+ _)
  implicit def forSet[W] = make[W, Set](Set, Set(_), _ + _)

  implicit def forArray[W: reflect.ClassTag]: AccumZero[W, Array[W]] =
    new AccumZero[W, Array[W]] {
      override def zero: Array[W] = Array.empty[W]
      override def one(w: W): Array[W] = Array[W](w)
      override def plus(fw1: Array[W], fw2: Array[W]): Array[W] = fw1 ++ fw2
      override def plus1(fw: Array[W], w: W): Array[W] = fw :+ w
    }

  private def make[W, F[X] <: Iterable[X]](
    factory: IterableFactory[F],
    singleton: W => F[W],
    addOne: (F[W], W) => F[W],
  ): AccumZero[W, F[W]] = 
    new AccumZero[W, F[W]] {
      override def zero: F[W] = factory.empty
      override def one(w: W): F[W] = singleton(w)
      override def plus(fw1: F[W], fw2: F[W]): F[W] = factory.concat(fw1, fw2)
      override def plus1(fw: F[W], w: W): F[W] = addOne(fw, w)
    }
}
