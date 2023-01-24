package turbolift.internals.extensions.orphaned
import turbolift.!!
import scala.collection.BuildFrom


/** No need to use this trait directly, because it's inherited by [[turbolift.Extensions Extensions]] object. */
/*private[turbolift]*/ trait TraverseExtensions extends AuxExtensions:
  extension [A, U, S[+X] <: IterableOnce[X]](thiz: S[A !! U])
    /** Like [[traversePar]], but discards the result. */
    def traverseVoidPar: Unit !! U =
      thiz.iterator.foldLeft(unit0[U])((mas, ma) => mas.zipWithPar(ma)(funAddVoid))

    /** Like [[traverse]], but discards the result. */
    def traverseVoid: Unit !! U =
      // thiz.iterator.foldLeft(unit0[U])((mas, ma) => mas.zipWith(ma)(funAddVoid))
      val iter = thiz.iterator
      def loop(): Unit !! U =
        if iter.hasNext
        then iter.next().flatMap(_ => loop())
        else !!.unit
      loop()


  extension [A, U](thiz: Iterator[A !! U])
    /** Transforms sequence of computations, into computation of sequence. */
    def traverse: Vector[A] !! U =
      thiz.foldLeft(empty0[A, U])((mas, ma) => mas.zipWith(ma)(funAddOne))

    /** Like [[traverse]], but executed parallelly for each element. */
    def traversePar: Vector[A] !! U =
      thiz.foldLeft(empty0[A, U])((mas, ma) => mas.zipWithPar(ma)(funAddOne))


  extension [A, U, S[+X] <: Iterable[X]](thiz: S[A !! U])(using BuildFrom[S[A !! U], A, S[A]])
    def traverse: S[A] !! U = doBuildFromVector(thiz, thiz.iterator.traverse)
    def traversePar: S[A] !! U = doBuildFromVector(thiz, thiz.iterator.traversePar)


  extension [A, U](thiz: Option[A !! U])
    /** Transforms option of computation, into computation of option. */
    def traverse: Option[A] !! U =
      thiz match
        case Some(ma) => ma.map(Some(_))
        case None => !!.pure(None)

    /** Like [[traverse]]. */
    def traversePar: Option[A] !! U = traverse

    /** Like [[traverse]], but discards the result. */
    def traverseVoid: Unit !! U =
      thiz match
        case Some(ma) => ma.void
        case None => !!.unit

    /** Like [[traverseVoid]]. */
    def traverseVoidPar: Unit !! U = traverseVoid


  extension [A, T, U](thiz: Either[T, A !! U])
    def traverse: Either[T, A] !! U =
      thiz match
        case Right(ma) => ma.map(Right(_))
        case Left(x) => !!.pure(Left(x))

    def traversePar: Either[T, A] !! U = traverse

    /** Like [[traverse]], but discards the result. */
    def traverseVoid: Unit !! U =
      thiz match
        case Right(ma) => ma.void
        case Left(_) => !!.unit

    /** Like [[traversePar]], but discards the result. */
    def traverseVoidPar: Unit !! U = traverseVoid
