package turbolift.internals.effect
import turbolift.Signature


private[turbolift] trait HasSignature extends Signature:
  type ThisSignature <: Signature
