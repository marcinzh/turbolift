package turbolift.internals.extensions.orphaned
import turbolift.!!


private[turbolift] trait FoldExtensions:
  extension [A, S[X] <: IterableOnce[X]](thiz: S[A])
    /** Like `foldLeft` from the standard library, but using effectful function. */
    def foldLeft_!![U, B](z: B)(op: (B, A) => B !! U): B !! U =
      thiz.iterator.foldLeft(!!.pure(z).upCast[U])((mb, a) => mb.flatMap(op(_, a)))

    /** Like `foldRight` from the standard library, but using effectful function. */
    def foldRight_!![U, B](z: B)(op: (A, B) => B !! U): B !! U =
      thiz.iterator.foldRight(!!.pure(z).upCast[U])((a, mb) => mb.flatMap(op(a, _)))

    /** Like `reduceLeft` from the standard library, but using effectful function. */
    def reduceLeft_!![U](op: (A, A) => A !! U): A !! U =
      val it = thiz.iterator
      val z = it.next()
      it.foldLeft_!!(z)(op)

    /** Like `reduceLeftOption` from the standard library, but using effectful function. */
    def reduceLeftOption_!![U](op: (A, A) => A !! U): Option[A] !! U =
      if thiz.iterator.isEmpty then !!.pure(None) 
      else reduceLeft_!!(op).map(Some(_))


  extension [A, S[X] <: Iterable[X]](thiz: S[A])
    /** Like `reduceRight` from the standard library, but using effectful function. */
    def reduceRight_!![U](op: (A, A) => A !! U): A !! U =
      thiz.init.foldRight_!!(thiz.last)(op)

    /** Like `reduceRightOption` from the standard library, but using effectful function. */
    def reduceRightOption_!![U](op: (A, A) => A !! U): Option[A] !! U =
      if thiz.nonEmpty then
        reduceRight_!!(op).map(Some(_))
      else
        !!.pure(None)

    /** Like `reduce` from the standard library, but using effectful function. */
    def reduce_!![U](op: (A, A) => A !! U): A !! U =
      assert(thiz.nonEmpty)
      def loop(as: Iterable[A]): A !! U =
        as.size match
          case 1 => !!.pure(as.head)
          case n =>
            val (as1, as2) = as.splitAt(n / 2)
            (loop(as1) *! loop(as2)).flatMap(op.tupled)
      loop(thiz)

    /** Like `reduceOption` from the standard library, but using effectful function. */
    def reduceOption_!![U](op: (A, A) => A !! U): Option[A] !! U =
      if thiz.nonEmpty then
        reduce_!!(op).map(Some(_))
      else
        !!.pure(None)

    /** [[map_!!]] fused with [[reduce_!!]]. */
    def mapReduce_!![B, U](f: A => B !! U)(op: (B, B) => B): B !! U =
      assert(thiz.nonEmpty)
      def loop(as: Iterable[A]): B !! U =
        as.size match
          case 1 => f(as.head)
          case n =>
            val (as1, as2) = as.splitAt(n / 2)
            loop(as1).zipWithPar(loop(as2))(op)
      loop(thiz)

    /** [[map_!!]] fused with [[reduceOption_!!]]. */
    def mapReduceOption_!![B, U](f: A => B !! U)(op: (B, B) => B): Option[B] !! U =
      if thiz.nonEmpty then
        mapReduce_!!(f)(op).map(Some(_))
      else
        !!.pure(None)

    //@#@
    def mapFold_!![B, U](f: A => B !! U)(z: B)(op: (B, B) => B): B !! U =
      if thiz.nonEmpty then
        thiz.mapReduce_!!(f)(op)
      else
        !!.pure(z)
