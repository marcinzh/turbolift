package turbolift.utils
import turbolift.abstraction.!!


trait FoldImplicits {
  implicit class Fold_IterableOnceOfComputationExtension[A, C[X] <: IterableOnce[X]](thiz: C[A]) {
    def foldLeft_!![U, B](z: B)(op: (B, A) => B !! U): B !! U =
      thiz.iterator.foldLeft(!!.pure(z).upCast[U]) {
        case (b_!, a) => for {
          b <- b_!
          b2 <- op(b, a)
        } yield b2
      }

    def reduceLeft_!![U](op: (A, A) => A !! U): A !! U = {
      val it = thiz.iterator
      val z = it.next()
      it.foldLeft_!!(z)(op)
    }

    def reduceLeftOption_!![U](op: (A, A) => A !! U): Option[A] !! U =
      if (thiz.iterator.isEmpty)
        !!.pure(None) 
      else 
        reduceLeft_!!(op).map(Some(_))
  }


  implicit class Fold_IterableOfComputationExtension[A, C[X] <: Iterable[X]](thiz: C[A]) {
    def foldRight_!![U, B](z: B)(op: (A, B) => B !! U): B !! U =
      thiz.foldRight(!!.pure(z).upCast[U]) {
        case (a, b_!) => for {
          b <- b_!
          b2 <- op(a, b)
        } yield b2
      }

    def reduceRight_!![U](op: (A, A) => A !! U): A !! U =
      thiz.init.foldRight_!!(thiz.last)(op)

    def reduceRightOption_!![U](op: (A, A) => A !! U): Option[A] !! U =
      if (thiz.isEmpty)
        !!.pure(None)
      else
        reduceRight_!!(op).map(Some(_))
  }
}
