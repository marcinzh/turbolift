package turbolift.internals.engine


private opaque type StoreSegment = StoreSegment.Underlying


private object StoreSegment extends StoreSegment_opaque:
  type Underlying = Array[Local]

  inline def wrap(x: Underlying): StoreSegment = x

  extension (x: StoreSegment)
    inline def unwrap: Underlying = x
