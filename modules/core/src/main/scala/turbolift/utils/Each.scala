package turbolift.utils
import turbolift.abstraction.!!
import turbolift.std_effects.Choice


case object Each extends Choice {
  def void = handler.void
}

trait EachExports {
  type Each = Each.type
}

trait EachExtensions {
  implicit class EachIteratorExtension[A](thiz: Iterator[A]) {
    def each_!! : A !! Each = Each.each(thiz.toVector)
  }

  implicit class EachIterableExtension[S[X] <: Iterable[X], A](thiz: S[A]) {
    def each_!! : A !! Each = Each.each(thiz)
  }
}
