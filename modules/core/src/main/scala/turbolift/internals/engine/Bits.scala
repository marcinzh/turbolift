package turbolift.internals.engine


private[engine] object Bits:
  
  //// Common: (UNSHIFTED)

  inline val Racer_None  = 0
  inline val Racer_Left  = 1
  inline val Racer_Right = 2
  inline val Racer_Both  = 3
  inline val Racer_Mask  = 0x3

  //// Same for constantBits and varyingBits
  def getRacer(bits: Int): Int = bits & Racer_Mask

  //// constantBits: [reentry ; tree ; racer]

  inline val Tree_Root   =   0 << Tree_Shift
  inline val Tree_ZipPar =   1 << Tree_Shift
  inline val Tree_OrPar  =   2 << Tree_Shift
  inline val Tree_OrSeq  =   3 << Tree_Shift
  inline val Tree_Mask   = 0x7 << Tree_Shift
  inline val Tree_Shift  = 2

  inline val Const_Reentry = 0x80

  def isRoot(bits: Int): Boolean = (bits & Tree_Mask) == Tree_Root
  def isReentry(bits: Int): Boolean = (bits & Const_Reentry) != 0

  inline def ZipPar_Left  = (Tree_ZipPar | Racer_Left).toByte
  inline def ZipPar_Right = (Tree_ZipPar | Racer_Right).toByte
  inline def OrPar_Left   = (Tree_OrPar | Racer_Left).toByte
  inline def OrPar_Right  = (Tree_OrPar | Racer_Right).toByte
  inline def OrSeq        = (Tree_OrSeq | Racer_Left).toByte

  //// varyingBits:  [cancellation ; completion ; racer]

  inline val Completion_Pending    = 0   << Completion_Shift
  inline val Completion_Success    = 1   << Completion_Shift
  inline val Completion_Cancelled  = 2   << Completion_Shift
  inline val Completion_Failure    = 3   << Completion_Shift
  inline val Completion_Mask       = 0x3 << Completion_Shift
  inline val Completion_Shift      = 2

  inline val Cancellation_Sent       = 0x1 << Cancellation_Shift
  inline val Cancellation_Received   = 0x2 << Cancellation_Shift
  inline val Cancellation_Suppressed = 0x4 << Cancellation_Shift
  inline val Cancellation_Mask       = 0x7 << Cancellation_Shift
  inline val Cancellation_Shift      = 4

  def isPending(bits: Int): Boolean = (bits & Completion_Mask) == 0
  def isCancellationSent(bits: Int): Boolean = (bits & Cancellation_Sent) != 0
  def isCancellationReceived(bits: Int): Boolean = (bits & Cancellation_Received) != 0
  def isCancellationUnreceived(bits: Int): Boolean = (bits & (Cancellation_Sent | Cancellation_Received)) == Cancellation_Sent

  //// raced

  private inline val Raced_S = (Completion_Success   >>> Completion_Shift) - 1
  private inline val Raced_C = (Completion_Cancelled >>> Completion_Shift) - 1
  private inline val Raced_F = (Completion_Failure   >>> Completion_Shift) - 1
  
  inline val Raced_SS = Raced_S + Raced_S * 3
  inline val Raced_SC = Raced_S + Raced_C * 3
  inline val Raced_SF = Raced_S + Raced_F * 3
  inline val Raced_CS = Raced_C + Raced_S * 3
  inline val Raced_CC = Raced_C + Raced_C * 3
  inline val Raced_CF = Raced_C + Raced_F * 3
  inline val Raced_FS = Raced_F + Raced_S * 3
  inline val Raced_FC = Raced_F + Raced_C * 3
  inline val Raced_FF = Raced_F + Raced_F * 3

  def makeRacedPair(completionLeft: Int, completionRight: Int) =
    val a = (completionLeft >>> Completion_Shift) - 1
    val b = (completionRight >>> Completion_Shift) - 1
    a + b * 3 
