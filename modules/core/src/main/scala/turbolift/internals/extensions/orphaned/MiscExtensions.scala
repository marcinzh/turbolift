package turbolift.internals.extensions.orphaned
import turbolift.!!
import turbolift.effects.Each


/** No need to use this trait directly, because it's inherited by [[turbolift.Extensions Extensions]] object. */
/*private[turbolift]*/ trait MiscExtensions:
  extension [A](thiz: A)
    /** Postfix alias of `pure(_)` */
    def pure_!! : A !! Any = !!.pure(thiz)

  extension [A](thiz: Iterable[A])
    def each_!! : A !! Each = Each.choose(thiz)

  extension [A](thiz: Iterator[A])
    def each_!! : A !! Each = Each.choose(thiz.toVector)
