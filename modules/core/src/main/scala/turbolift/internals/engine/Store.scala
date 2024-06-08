package turbolift.internals.engine


private opaque type Store = Store.Underlying


private object Store extends Store_opaque:
  type Underlying = StoreNel | StoreSegment

  inline def wrap(x: Underlying): Store = x

  extension (x: Store)
    inline def unwrap: Underlying = x


extension (thiz: (StoreSegment, StoreSegment))
  private inline def asPairOfStores: (Store, Store) = thiz
