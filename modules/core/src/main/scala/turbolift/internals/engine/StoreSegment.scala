package turbolift.internals.engine


private[engine] opaque type StoreSegment = StoreSegment.Underlying


private[engine] object StoreSegment extends StoreSegment_opaque:
  type Underlying = Array[Stan]

  inline def wrap(x: Underlying): StoreSegment = x

  extension (x: StoreSegment)
    inline def unwrap: Underlying = x
