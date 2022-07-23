package turbolift.internals.effect
import turbolift.Signature


private[turbolift] trait HasSignature extends Signature:
  private[turbolift] type ThisSignature <: Signature
