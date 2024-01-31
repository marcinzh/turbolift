package turbolift.internals.engine
import turbolift.io.Warp


private[internals] abstract class WarpLink:
  protected var linkLeft: WarpLink | Null = null
  protected var linkRight: WarpLink | Null = null
