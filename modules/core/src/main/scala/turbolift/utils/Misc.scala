package turbolift.utils
import turbolift.abstraction.!!
import turbolift.std_effects.Each


trait MiscExtensions {
  implicit class MiscIteratorExtension[A](thiz: Iterator[A]) {
    def each_!! : A !! Each = Each.each(thiz.toVector)
  }

  implicit class MiscIterableExtension[A](thiz: Iterable[A]) {
    def each_!! : A !! Each = Each.each(thiz)
  }

  implicit class MiscAnyExtension[A](thiz: A) {
    def pure_!! : A !! Any = !!.pure(thiz)
  }
}
