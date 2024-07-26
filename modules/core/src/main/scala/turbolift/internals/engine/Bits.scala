package turbolift.internals.engine


private object Bits:
  
  //// Common: (UNSHIFTED)

  inline val Racer_None  = 0
  inline val Racer_Left  = 1
  inline val Racer_Right = 2
  inline val Racer_Both  = 3
  inline val Racer_Mask  = 0x3

  //// constantBits:
  ////     racer   = 2
  ////     tree    = 3
  ////     _       = 2
  ////     reentry = 1

  inline val Tree_Root     =   0 << Tree_Shift
  inline val Tree_Explicit =   1 << Tree_Shift
  inline val Tree_ZipPar   =   2 << Tree_Shift
  inline val Tree_OrPar    =   3 << Tree_Shift
  inline val Tree_OrSeq    =   4 << Tree_Shift
  inline val Tree_Mask     = 0x7 << Tree_Shift
  inline val Tree_Shift    = 2

  inline val Const_Reentry = 0x80

  def isRoot(bits: Int): Boolean = (bits & Tree_Mask) == Tree_Root
  def isExplicit(bits: Int): Boolean = (bits & Tree_Mask) == Tree_Explicit
  def isReentry(bits: Int): Boolean = (bits & Const_Reentry) != 0

  inline def ZipPar_Left  = (Tree_ZipPar | Racer_Left).toByte
  inline def ZipPar_Right = (Tree_ZipPar | Racer_Right).toByte
  inline def OrPar_Left   = (Tree_OrPar | Racer_Left).toByte
  inline def OrPar_Right  = (Tree_OrPar | Racer_Right).toByte
  inline def OrSeq        = (Tree_OrSeq | Racer_Left).toByte

  //// theOwnership:

  inline val Ownership_Self    = 0
  inline val Ownership_Waitee  = 1
  inline val Ownership_Blocker = 2

  //// varyingBits:
  ////    arbiter      = 2 bits
  ////    completion   = 2 bits
  ////    cancellation = 2 bits

  inline val Arbiter_None  = Racer_None
  inline val Arbiter_Left  = Racer_Left
  inline val Arbiter_Right = Racer_Right
  inline val Arbiter_Both  = Racer_Both
  inline val Arbiter_Mask  = Racer_Mask

  inline val Completion_Pending    =   0 << Completion_Shift
  inline val Completion_Success    =   1 << Completion_Shift
  inline val Completion_Cancelled  =   2 << Completion_Shift
  inline val Completion_Failure    =   3 << Completion_Shift
  inline val Completion_Mask       = 0x3 << Completion_Shift
  inline val Completion_Shift      = 2

  inline val Cancellation_Signal  = 0x1 << Cancellation_Shift
  inline val Cancellation_Latch   = 0x2 << Cancellation_Shift
  inline val Cancellation_Mask    = 0x3 << Cancellation_Shift
  inline val Cancellation_Shift   = 4

  //@#@ temporary duplicate until compiler bug is fixed
  final val Cancellation_Latch_Bug: Int = Cancellation_Latch

  def getCompletion(bits: Int): Int = bits & Completion_Mask
  def getArbiter(bits: Int): Int = bits & Arbiter_Mask
  def isPending(bits: Int): Boolean = getCompletion(bits) == Completion_Pending
  def isPendingAndNotCancelled(bits: Int): Boolean = (bits & (Completion_Mask | Cancellation_Signal)) == 0
  def isCancellationSignalled(bits: Int): Boolean = (bits & Cancellation_Signal) != 0
  def isCancellationUnlatched(bits: Int): Boolean = (bits & (Cancellation_Signal | Cancellation_Latch)) == Cancellation_Signal

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


  //// Waiter & Waitee

  inline val WaiterSubscribed = 0
  final val WaiterAlreadyCancelled = 1 //@#@ no `inline val` bcoz compiler bug
  inline val WaiteeAlreadyCompleted = 2


  //// Warp

  inline val Warp_Pending   = Completion_Pending
  inline val Warp_Completed = Completion_Success
  inline val Warp_Cancelled = Cancellation_Signal
  inline val Warp_Shutdown  = Cancellation_Latch

  inline val ExitMode_None = 0
  inline val ExitMode_Cancel = 1
  inline val ExitMode_Shutdown = 2

  def isShutdown(bits: Int): Boolean = (bits & Warp_Shutdown) != 0
