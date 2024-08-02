package turbolift.internals.engine.stacked


private[engine] opaque type Store = Store.Underlying


private[engine] object Store extends Store_opaque:
  type Underlying = Array[Any]

  inline def wrap(x: Underlying): Store = x

  extension (x: Store)
    inline def unwrap: Underlying = x
