package turbolift.internals.extensions.orphaned
import turbolift.!!
import scala.collection.BuildFrom


private[turbolift] trait MapFilterExtensions:
  extension [A](thiz: Iterator[A])
    def map_!![B, U](f: A => B !! U): Vector[B] !! U =
      thiz.iterator.foldLeft(!!.pure(Vector.empty[B]).upCast[U]) { (mbs, a) => 
        for
          bs <- mbs
          b <- f(a)
        yield bs :+ b
      }

    def flatMap_!![B, U](f: A => IterableOnce[B] !! U): Vector[B] !! U =
      thiz.iterator.foldLeft(!!.pure(Vector.empty[B]).upCast[U]) { (mbs, a) => 
        for
          bs <- mbs
          bs2 <- f(a)
        yield bs ++ bs2
      }

    def filter_!![U](f: A => Boolean !! U): Vector[A] !! U =
      thiz.iterator.foldLeft(!!.pure(Vector.empty[A]).upCast[U]) { (mas, a) => 
        for
          as <- mas
          c <- f(a)
        yield if c then as :+ a else as
      }


  extension [A](thiz: IterableOnce[A])
    def foreach_!![B, U](f: A => Unit !! U): Unit !! U =
      thiz.iterator.foldLeft(!!.pure(()).upCast[U]) { (mb, a) => mb &! f(a) }


  private type BF[A, B, S[_]] = BuildFrom[S[A], B, S[B]]

  extension [A, S[X] <: Iterable[X]](thiz: S[A])
    // :-(
    // type BF[X] = BuildFrom[S[A], X, S[X]]

    def map_!![B, U](f: A => B !! U)(using bf: BF[A, B, S]): S[B] !! U =
      thiz.iterator.map_!!(f)
      .map(as => (bf.newBuilder(thiz) ++= as).result())

    def flatMap_!![B, U](f: A => IterableOnce[B] !! U)(using bf: BF[A, B, S]): S[B] !! U =
      thiz.iterator.flatMap_!!(f)
      .map(as => (bf.newBuilder(thiz) ++= as).result())

    def filter_!![U](f: A => Boolean !! U)(using bf: BF[A, A, S]): S[A] !! U =
      thiz.iterator.filter_!!(f)
      .map(as => (bf.newBuilder(thiz) ++= as).result())
