package turbolift


trait Signature:
  type !@![A, U]
  type ThisEffect


private[turbolift] object Signature:
  type Apply[U] = Signature { type ThisEffect = U }
