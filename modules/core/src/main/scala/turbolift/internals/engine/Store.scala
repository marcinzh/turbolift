package turbolift.internals.engine


private opaque type Store = Store.Underlying


private object Store extends Store_opaque:
  type Underlying = Array[Local]

  inline def wrap(x: Underlying): Store = x

  extension (x: Store)
    inline def unwrap: Underlying = x
