package turbolift.internals.engine


private[engine] opaque type FrameKind = Int

private[engine] object FrameKind:
  inline def wrap(n: Int): FrameKind = n

  def plain: FrameKind = PLAIN
  def guard: FrameKind = GUARD
  def warp: FrameKind = WARP
  def suppress: FrameKind = SUPPRESS

  extension (thiz: FrameKind)
    inline def unwrap: Int = thiz

  inline val PLAIN    = 0
  inline val GUARD    = 1
  inline val WARP     = 2
  inline val SUPPRESS = 3
  //---------------------
  inline val BIT_COUNT = 2
  inline val MASK = (1 << BIT_COUNT) - 1
