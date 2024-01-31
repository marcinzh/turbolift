package turbolift.internals.extensions
import turbolift.!!

/** No need to use this trait directly, because it's inherited by [[turbolift.Extensions Extensions]] object. */
/*private[turbolift]*/ trait FoldExtensions:
  extension [A, S[X] <: IterableOnce[X]](thiz: S[A])
    /** Like `foldLeft` from the standard library, but using effectful function. */
    def foldLeftEff[U, B](z: B)(op: (B, A) => B !! U): B !! U =
      thiz.iterator.foldLeft(!!.pure(z).upCast[U])((mb, a) => mb.flatMap(op(_, a)))

    /** Like `foldRight` from the standard library, but using effectful function. */
    def foldRightEff[U, B](z: B)(op: (A, B) => B !! U): B !! U =
      thiz.iterator.foldRight(!!.pure(z).upCast[U])((a, mb) => mb.flatMap(op(a, _)))

    /** Like `reduceLeft` from the standard library, but using effectful function. */
    def reduceLeftEff[U](op: (A, A) => A !! U): A !! U =
      val it = thiz.iterator
      val z = it.next()
      it.foldLeftEff(z)(op)

    /** Like `reduceLeftOption` from the standard library, but using effectful function. */
    def reduceLeftOptionEff[U](op: (A, A) => A !! U): Option[A] !! U =
      if thiz.iterator.nonEmpty then
        reduceLeftEff(op).map(Some(_))
      else
        !!.none


  extension [A, S[X] <: Iterable[X]](thiz: S[A])
    /** Like `reduceRight` from the standard library, but using effectful function. */
    def reduceRightEff[U](op: (A, A) => A !! U): A !! U =
      thiz.init.foldRightEff(thiz.last)(op)

    /** Like `reduceRightOption` from the standard library, but using effectful function. */
    def reduceRightOptionEff[U](op: (A, A) => A !! U): Option[A] !! U =
      if thiz.nonEmpty then
        reduceRightEff(op).map(Some(_))
      else
        !!.none

    /** Like `reduce` from the standard library, but using effectful function. */
    def reduceEff[U](op: (A, A) => A !! U): A !! U =
      assert(thiz.nonEmpty)
      def loop(as: Iterable[A]): A !! U =
        as.size match
          case 1 => !!.pure(as.head)
          case n =>
            val (as1, as2) = as.splitAt(n / 2)
            (loop(as1) *! loop(as2)).flatMap(op.tupled)
      loop(thiz)

    /** Like `reduceOption` from the standard library, but using effectful function. */
    def reduceOptionEff[U](op: (A, A) => A !! U): Option[A] !! U =
      if thiz.nonEmpty then
        reduceEff(op).map(Some(_))
      else
        !!.none

    /** `mapEff` fused with [[reduceEff]]. */
    def mapReduceEff[B, U](f: A => B !! U)(op: (B, B) => B): B !! U =
      assert(thiz.nonEmpty)
      def loop(as: Iterable[A]): B !! U =
        as.size match
          case 1 => f(as.head)
          case n =>
            val (as1, as2) = as.splitAt(n / 2)
            loop(as1).zipWithPar(loop(as2))(op)
      loop(thiz)

    /** `mapEff` fused with [[reduceOptionEff]]. */
    def mapReduceOptionEff[B, U](f: A => B !! U)(op: (B, B) => B): Option[B] !! U =
      if thiz.nonEmpty then
        mapReduceEff(f)(op).map(Some(_))
      else
        !!.none

    //@#@
    def mapFoldEff[B, U](f: A => B !! U)(z: B)(op: (B, B) => B): B !! U =
      if thiz.nonEmpty then
        thiz.mapReduceEff(f)(op)
      else
        !!.pure(z)
