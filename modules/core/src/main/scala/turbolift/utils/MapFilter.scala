package turbolift.utils
import turbolift.abstraction.!!
import scala.collection.BuildFrom


trait MapFilterExtensions {
  implicit class MapFilterIteratorExtension[A](thiz: Iterator[A]) {
    def map_!![B, U](f: A => B !! U): Vector[B] !! U =
      thiz.iterator.foldLeft(!!.pure(Vector.empty[B]).upCast[U]) { (mbs, a) => 
        for {
          bs <- mbs
          b <- f(a)
        } yield bs :+ b
      }

    def flatMap_!![B, U](f: A => IterableOnce[B] !! U): Vector[B] !! U =
      thiz.iterator.foldLeft(!!.pure(Vector.empty[B]).upCast[U]) { (mbs, a) => 
        for {
          bs <- mbs
          bs2 <- f(a)
        } yield bs ++ bs2
      }

    def filter_!![U](f: A => Boolean !! U): Vector[A] !! U =
      thiz.iterator.foldLeft(!!.pure(Vector.empty[A]).upCast[U]) { (mas, a) => 
        for {
          as <- mas
          c <- f(a)
        } yield if (c) as :+ a else as
      }
  }

  implicit class MapIterableExtension[A, B, S[X] <: Iterable[X]](thiz: S[A])(implicit bf: BuildFrom[S[A], B, S[B]]) {
    def map_!![U](f: A => B !! U): S[B] !! U =
      thiz.iterator.map_!!(f)
      .map(as => (bf.newBuilder(thiz) ++= as).result())

    def flatMap_!![U](f: A => IterableOnce[B] !! U): S[B] !! U =
      thiz.iterator.flatMap_!!(f)
      .map(as => (bf.newBuilder(thiz) ++= as).result())
  }

  implicit class FilterIterableExtension[A, S[X] <: Iterable[X]](thiz: S[A])(implicit bf: BuildFrom[S[A], A, S[A]]) {
    def filter_!![U](f: A => Boolean !! U): S[A] !! U =
      thiz.iterator.filter_!!(f)
      .map(as => (bf.newBuilder(thiz) ++= as).result())
  }
}
