package turbolift.internals.engine


private[engine] opaque type FramePacked = Int

private[engine] object FramePacked:
  def apply(
    delta: Int,
    isLocal: Boolean,
    kind: FrameKind,
  ): FramePacked =
    assert(delta <= MAX_DELTA)
    (delta << SHIFT) +
    (if isLocal then LOCAL else 0) +
    kind.unwrap


  private inline val LOCAL = 1 << FrameKind.BIT_COUNT
  private inline val SHIFT = FrameKind.BIT_COUNT + 1
  private inline val MAX_DELTA = (1 << (32 - SHIFT)) - 1


  extension (thiz: FramePacked)
    def kind: FrameKind = FrameKind.wrap(thiz & FrameKind.MASK)
    def isLocal: Boolean = (thiz & LOCAL) != 0
    def isGuard: Boolean = (thiz & FrameKind.GUARD) != 0
    def delta: Int = thiz >>> SHIFT
    def clearDelta: FramePacked = thiz & ((1 << SHIFT) - 1)
