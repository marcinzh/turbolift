package turbolift.abstraction


trait Signature:
  type !@![A, U]
  type ThisEffect


object Signature:
  type Apply[U] = Signature { type ThisEffect = U }
