package turbolift.internals.extensions.orphaned
import turbolift.!!
import turbolift.std_effects.Each



trait MiscExtensions:
  extension [A](thiz: A)
    def pure_!! : A !! Any = !!.pure(thiz)

  extension [A](thiz: Iterable[A])
    def each_!! : A !! Each = Each.choose(thiz)

  extension [A](thiz: Iterator[A])
    def each_!! : A !! Each = Each.choose(thiz.toVector)
