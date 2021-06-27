package turbolift.abstraction.internals.interpreter


trait HasSignature:
  type Signature[U] <: AnyRef


type AnySignature[U] = HasSignature#Signature[U]
