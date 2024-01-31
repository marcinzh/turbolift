package turbolift.internals.engine


private[engine] object Bits:
  
  //// Common: (UNSHIFTED)

  inline val Child_None  = 0
  inline val Child_Left  = 1
  inline val Child_Right = 2
  inline val Child_Both  = 3
  inline val Child_Mask  = 3

  inline val Outcome_Pending    = 0
  inline val Outcome_Success    = 1
  inline val Outcome_Cancelled  = 2
  inline val Outcome_Failure    = 3
  inline val Outcome_Mask       = 0xf

  //// constantBits: [tree ; which_child_am_i]

  inline val Tree_Root   =   0 << Tree_Shift
  inline val Tree_ZipPar =   1 << Tree_Shift
  inline val Tree_OrPar  =   2 << Tree_Shift
  inline val Tree_OrSeq  =   3 << Tree_Shift
  inline val Tree_Mask   = 0xf << Tree_Shift
  inline val Tree_Shift  = 4
  def isRoot(bits: Int): Boolean = (bits & Tree_Mask) == Tree_Root

  inline def ZipPar_Left  = (Tree_ZipPar | Child_Left).toByte
  inline def ZipPar_Right = (Tree_ZipPar | Child_Right).toByte
  inline def OrPar_Left   = (Tree_OrPar | Child_Left).toByte
  inline def OrPar_Right  = (Tree_OrPar | Child_Right).toByte
  inline def OrSeq        = (Tree_OrSeq | Child_Left).toByte

  //// varyingBits:  [other ; cancellation ; completion ; winner-outcome ; awaiting-child]

  //@#@TODO no longer needed, just use Child_XXX
  inline val Awaiting_None  = Child_None  << Awaiting_Shift 
  inline val Awaiting_Left  = Child_Left  << Awaiting_Shift 
  inline val Awaiting_Right = Child_Right << Awaiting_Shift 
  inline val Awaiting_Both  = Child_Both  << Awaiting_Shift 
  inline val Awaiting_Mask  = Child_Mask  << Awaiting_Shift 
  inline val Awaiting_Shift = 0

  inline val Winner_Success    = Outcome_Success   << Winner_Shift
  inline val Winner_Cancelled  = Outcome_Cancelled << Winner_Shift
  inline val Winner_Failure    = Outcome_Failure   << Winner_Shift
  inline val Winner_Mask       = Outcome_Mask      << Winner_Shift
  inline val Winner_Shift      = 4

  inline val Completion_Pending    = Outcome_Pending   << Completion_Shift
  inline val Completion_Success    = Outcome_Success   << Completion_Shift
  inline val Completion_Cancelled  = Outcome_Cancelled << Completion_Shift
  inline val Completion_Failure    = Outcome_Failure   << Completion_Shift
  inline val Completion_Mask       = Outcome_Mask      << Completion_Shift
  inline val Completion_Shift      = 8

  inline val Cancellation_Sent       = 0x1 << Cancellation_Shift
  inline val Cancellation_Received   = 0x2 << Cancellation_Shift
  inline val Cancellation_Mask       = 0xf << Cancellation_Shift
  inline val Cancellation_Shift      = 12

  inline val Other_Substitute = 0x1 << Other_Shift 
  inline val Other_Shift      = 16

  def isPending(bits: Int): Boolean = (bits & Completion_Mask) == 0
  def isSubstitute(bits: Int): Boolean = (bits & Other_Substitute) != 0
  def isCancellationSent(bits: Int): Boolean = (bits & Cancellation_Sent) != 0
  def isCancellationReceived(bits: Int): Boolean = (bits & Cancellation_Received) != 0
  def isCancellationUnreceived(bits: Int): Boolean = (bits & (Cancellation_Sent | Cancellation_Received)) == Cancellation_Sent

  //// raced

  private inline val Raced_S = Outcome_Success - 1
  private inline val Raced_C = Outcome_Cancelled - 1
  private inline val Raced_F = Outcome_Failure - 1
  
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
