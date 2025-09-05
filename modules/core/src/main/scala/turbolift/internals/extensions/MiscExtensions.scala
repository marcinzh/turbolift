package turbolift.internals.extensions
import turbolift.!!
import turbolift.effects.{Error, Each}


/** No need to use this trait directly, because it's inherited by [[turbolift.Extensions Extensions]] object. */
/*private[turbolift]*/ trait MiscExtensions:
  extension [A](thiz: A)
    /** Postfix alias of `pure(_)` */
    def pure_!! : A !! Any = !!.pure(thiz)
    def raiseError: Nothing !! Error[A] = Error.raise(thiz)

  extension [A](thiz: Iterable[A])
    def each_!! : A !! Each = Each.choose(thiz)

  extension [A](thiz: Iterator[A])
    def each_!! : A !! Each = Each.choose(thiz.toVector)

  extension [A](thiz: Option[A])
    def getOrElseEff[A2 >: A, U](comp: => A2 !! U): A2 !! U =
      thiz.fold(comp)(!!.pure)

  extension [A, E](thiz: Either[E, A])
    def getOrElseEff[A2 >: A, U](comp: E => A2 !! U): A2 !! U =
      thiz.fold(comp, !!.pure)
