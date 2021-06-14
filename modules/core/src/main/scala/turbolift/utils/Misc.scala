package turbolift.utils
import turbolift.abstraction.!!
import turbolift.std_effects.Each


trait MiscExtensions:
  extension [A](thiz: Iterator[A])
    def each_!! : A !! Each = Each.each(thiz.toVector)

  extension [A](thiz: Iterable[A])
    def each_!! : A !! Each = Each.each(thiz)

  extension [A](thiz: A)
    def pure_!! : A !! Any = !!.pure(thiz)
