package turbolift.internals.interpreter


private[internals] object Bits:
  inline val IsStateful        = 0x1
  inline val IsProxy           = 0x2
  inline val IsProxyIO         = 0x4
  inline val IsSequential      = 0x8
  inline val IsChoice          = 0x10
  inline val HasZip            = 0x20
  inline val HasForkJoin       = 0x40
  inline val HasUnpure         = 0x80
