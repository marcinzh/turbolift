package turbolift.internals.extensions.orphaned
import turbolift.!!


trait MiscExtensions:
  extension [A](thiz: A)
    def pure_!! : A !! Any = !!.pure(thiz)
