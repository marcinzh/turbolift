package turbolift.typeclass


object Syntax:
  extension [T](thiz: T)
    def |+|(using ev: Plus[T])(that: T): T = ev.plus(thiz, that)
    def |+[O](using ev: Accum[T, O])(that: O): T = ev.plus1(thiz, that)
