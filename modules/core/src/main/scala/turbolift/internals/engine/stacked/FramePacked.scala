package turbolift.internals.engine.stacked


private opaque type FramePacked = Int

private object FramePacked:
  def apply(
    delta: Int,
    isNested: Boolean,
    kind: FrameKind,
  ): FramePacked =
    assert(delta <= MAX_DELTA)
    (delta << SHIFT) +
    (if isNested then NESTED else 0) +
    kind.unwrap


  private inline val NESTED = 1 << FrameKind.BIT_COUNT
  private inline val SHIFT = FrameKind.BIT_COUNT + 1
  private inline val MAX_DELTA = (1 << (32 - SHIFT)) - 1


  extension (thiz: FramePacked)
    def kind: FrameKind = FrameKind.wrap(thiz & FrameKind.MASK)
    def isNested: Boolean = (thiz & NESTED) != 0
    def isGuard: Boolean = (thiz & FrameKind.GUARD) != 0
    def delta: Int = thiz >>> SHIFT
    def clearDelta: FramePacked = thiz & ((1 << SHIFT) - 1)
