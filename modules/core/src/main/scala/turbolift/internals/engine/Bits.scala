package turbolift.internals.engine


private[engine] object Bits:
  
  //// Common:

  final val Child_None  = 0
  final val Child_Left  = 1
  final val Child_Right = 2
  final val Child_Both  = 3
  final val Child_Mask  = 3

  //// constantBits: [tree ; child]

  final val Tree_Root = 0   << Tree_Shift
  final val Tree_Zip  = 1   << Tree_Shift
  final val Tree_Race = 2   << Tree_Shift
  final val Tree_Mask = 0xf << Tree_Shift
  final val Tree_Shift = 4
  def isRoot(bits: Int): Boolean = (bits & Tree_Mask) == Tree_Root

  final val Zip_Left  = (Tree_Zip | Child_Left).toByte
  final val Zip_Right = (Tree_Zip | Child_Right).toByte

  //// varyingBits:  [cancellation ; phase ; awaited-child ; successful-child]

  final val Awaiting_None  = Child_None  << Awaiting_Shift 
  final val Awaiting_Left  = Child_Left  << Awaiting_Shift 
  final val Awaiting_Right = Child_Right << Awaiting_Shift 
  final val Awaiting_Both  = Child_Both  << Awaiting_Shift 
  final val Awaiting_Mask  = Child_Mask  << Awaiting_Shift 
  final val Awaiting_Shift = 4

  final val Phase_Running = 0x0 << Phase_Shift 
  final val Phase_Booked  = 0x1 << Phase_Shift
  final val Phase_Success = 0x2 << Phase_Shift
  final val Phase_Failure = 0x4 << Phase_Shift
  final val Phase_Mask    = 0xf << Phase_Shift
  final val Phase_Shift   = 8

  def isPending(bits: Int): Boolean = (bits & (Phase_Success | Phase_Failure)) == 0
  def isSuccess(bits: Int): Boolean = (bits & Phase_Success) != 0

  final val Cancellation_Requested = 0x1 << Cancellation_Shift
  final val Cancellation_Latched   = 0x2 << Cancellation_Shift
  final val Cancellation_Mask      = 0xf << Cancellation_Shift
  final val Cancellation_Shift     = 12

  def isCancelled(bits: Int): Boolean = (bits & Cancellation_Requested) != 0
  def isLatched(bits: Int): Boolean = (bits & Cancellation_Latched) != 0
  def isCancelledButNotLatched(bits: Int): Boolean = (bits & Cancellation_Mask) == Cancellation_Requested

  final val Other_Shift = 16
  final val Substitute = 0x1 << Other_Shift
  def isSubstitute(bits: Int): Boolean = (bits & Substitute) != 0

  //// returned by `tryWin`

  final val Winner = 1 << 31
  final val Loser = 0
  def isWinner(bits: Int): Boolean = bits < 0
