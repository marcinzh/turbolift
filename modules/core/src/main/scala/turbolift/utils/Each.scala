package turbolift.utils
import turbolift.abstraction.!!
import turbolift.std_effects.Each


trait EachExtensions {
  implicit class EachIteratorExtension[A](thiz: Iterator[A]) {
    def each_!! : A !! Each = Each.each(thiz.toVector)
  }

  implicit class EachIterableExtension[A](thiz: Iterable[A]) {
    def each_!! : A !! Each = Each.each(thiz)
  }
}
