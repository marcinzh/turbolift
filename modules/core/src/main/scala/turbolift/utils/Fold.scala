package turbolift.utils
import turbolift.abstraction._


trait FoldExports {
  implicit class Fold_TraversableOnceOfComputationExtension[A, C[X] <: TraversableOnce[X]](thiz: C[A]) {
    def foldLeft_!![U, B](z: B)(op: (B, A) => B !! U): B !! U =
      thiz.foldLeft(Return(z).widen[U]) {
        case (b_!, a) => for {
          b <- b_!
          b2 <- op(b, a)
        } yield b2
      }

    def reduceLeft_!![U](op: (A, A) => A !! U): A !! U = {
      val it = thiz.toIterator
      val z = it.next()
      it.foldLeft_!!(z)(op)
    }

    def reduceLeftOption_!![U](op: (A, A) => A !! U): Option[A] !! U =
      if (thiz.isEmpty)
        Return(None) 
      else 
        reduceLeft_!!(op).map(Some(_))
  }


  implicit class Fold_IterableOfComputationExtension[A, C[X] <: Iterable[X]](thiz: C[A]) {
    def foldRight_!![U, B](z: B)(op: (A, B) => B !! U): B !! U =
      thiz.foldRight(Return(z).widen[U]) {
        case (a, b_!) => for {
          b <- b_!
          b2 <- op(a, b)
        } yield b2
      }

    def reduceRight_!![U](op: (A, A) => A !! U): A !! U =
      thiz.init.foldRight_!!(thiz.last)(op)

    def reduceRightOption_!![U](op: (A, A) => A !! U): Option[A] !! U =
      if (thiz.isEmpty)
        Return(None)
      else
        reduceRight_!!(op).map(Some(_))
  }
}
