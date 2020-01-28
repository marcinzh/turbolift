package turbolift.utils
import turbolift.abstraction.!!
import scala.collection.generic.CanBuildFrom


trait TraverseExports {
  implicit class IterableOfComputationExtension[+A, -U, S[+X] <: Iterable[X]](thiz: S[A !! U]) {
    def traverseVoid: Unit !! U = thiz.foldLeft(!!.pure().upCast[U])(_ *<! _)

    def traverseVoidShort: Unit !! U = {
      def loop(todos: Iterable[A !! U]): Unit !! U =
        if (todos.isEmpty)
          !!.pure()
        else 
          todos.head.flatMap(_ => loop(todos.tail))

      loop(thiz)
    }
  }


  implicit class IterableOfComputationCBFExtension[+A, -U, S[+X] <: Iterable[X]](thiz: S[A !! U])(implicit cbf: CanBuildFrom[S[A !! U], A, S[A]]) {
    def traverse: S[A] !! U = {
      def loop(as: Iterable[A !! U]): Vector[A] !! U =
        as.size match {
          case 0 => !!.pure(Vector.empty[A])
          case 1 => as.head.map(Vector(_))
          case n =>
            val (as1, as2) = as.splitAt(n / 2)
            (loop(as1) *! loop(as2)).map { case (xs, ys) => xs ++ ys }
        }
      loop(thiz)
      .map(as => (cbf() ++= as).result())
    }

    def traverseShort: S[A] !! U = {
      def loop(todos: Iterable[A !! U], accum: Vector[A]): Vector[A] !! U =
        if (todos.isEmpty)
          !!.pure(accum)
        else 
          todos.head.flatMap(a => loop(todos.tail, accum :+ a))

      loop(thiz, Vector())
      .map(as => (cbf() ++= as).result())
    }
  }

  implicit class OptionOfComputationExtension[+A, -U](thiz: Option[A !! U]) {
    def traverse: Option[A] !! U =
      thiz match {
        case Some(ma) => ma.map(Some(_))
        case None => !!.pure(None)
      }
    def traverseVoid: Unit !! U =
      thiz match {
        case Some(ma) => ma.void
        case None => !!.pure()
      }
    def traverseShort = traverse
    def traverseVoidShort = traverseVoid
  }


  implicit class EitherOfComputationExtension[+A, +T, -U](thiz: Either[T, A !! U]) {
    def traverse: Either[T, A] !! U =
      thiz match {
        case Right(ma) => ma.map(Right(_))
        case Left(x) => !!.pure(Left(x))
      }
    def traverseVoid: Unit !! U =
      thiz match {
        case Right(ma) => ma.void
        case Left(_) => !!.pure()
      }
    def traverseShort = traverse
    def traverseVoidShort = traverseVoid
  }
}
