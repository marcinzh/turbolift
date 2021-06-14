package turbolift.abstraction.internals.interpreter
import turbolift.abstraction.internals.effect.HasEffectId


trait HasSignature extends HasEffectId.Delegate:
  type Signature[U] <: AnyRef


type AnySignature[U] = HasSignature#Signature[U]
