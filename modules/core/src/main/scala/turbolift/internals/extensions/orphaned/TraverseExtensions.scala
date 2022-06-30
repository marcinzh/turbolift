package turbolift.internals.extensions.orphaned
import turbolift.!!
import scala.collection.BuildFrom


trait TraverseExtensions:
  extension [A, U, S[+X] <: IterableOnce[X]](thiz: S[A !! U])
    def traverseVoid: Unit !! U =
      thiz.iterator.foldLeft(!!.pure().upCast[U])(_ &<! _)

    def traverseShortVoid: Unit !! U =
      val iter = thiz.iterator
      def loop(): Unit !! U =
        if iter.hasNext
        then iter.next().flatMap(_ => loop())
        else !!.pure()
      loop()


  extension [A, U](thiz: Iterator[A !! U])
    def traverse: Vector[A] !! U =
      thiz.foldLeft(!!.pure(Vector.empty[A]).upCast[U]) { case (mas, ma) => (mas *! ma).map { case (as, a) => as :+ a }}

    def traverseShort: Vector[A] !! U =
      val iter = thiz.iterator
      def loop(accum: Vector[A]): Vector[A] !! U =
        if iter.hasNext
        then iter.next().flatMap(a => loop(accum :+ a))
        else !!.pure(accum)
      loop(Vector())


  extension [A, U, S[+X] <: Iterable[X]](thiz: S[A !! U])(using bf: BuildFrom[S[A !! U], A, S[A]])
    def traverse: S[A] !! U =
      def loop(as: Iterable[A !! U]): Vector[A] !! U =
        as.size match
          case 0 => !!.pure(Vector.empty[A])
          case 1 => as.head.map(Vector(_))
          case n =>
            val (as1, as2) = as.splitAt(n / 2)
            (loop(as1) *! loop(as2)).map { case (xs, ys) => xs ++ ys }
      loop(thiz)
      .map(as => (bf.newBuilder(thiz) ++= as).result())

    def traverseShort: S[A] !! U =
      thiz.iterator.traverseShort
      .map(as => (bf.newBuilder(thiz) ++= as).result())


  extension [A, U](thiz: Option[A !! U])
    def traverse: Option[A] !! U =
      thiz match
        case Some(ma) => ma.map(Some(_))
        case None => !!.pure(None)

    def traverseVoid: Unit !! U =
      thiz match
        case Some(ma) => ma.void
        case None => !!.pure()

    def traverseShort: Option[A] !! U = traverse
    def traverseShortVoid: Unit !! U = traverseVoid


  extension [A, T, U](thiz: Either[T, A !! U])
    def traverse: Either[T, A] !! U =
      thiz match
        case Right(ma) => ma.map(Right(_))
        case Left(x) => !!.pure(Left(x))

    def traverseVoid: Unit !! U =
      thiz match
        case Right(ma) => ma.void
        case Left(_) => !!.pure()

    def traverseShort: Either[T, A] !! U = traverse
    def traverseShortVoid: Unit !! U = traverseVoid
