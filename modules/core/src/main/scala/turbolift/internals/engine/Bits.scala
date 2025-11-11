package turbolift.internals.engine


//@#@ public bcoz inline problems
object Bits:
  inline val Kind_Main       = 0 //// isRoot
  inline val Kind_Reentry    = 1 //// isRoot
  inline val Kind_Explicit   = 2
  inline val Kind_RaceAll    = 3 //// isRacer
  inline val Kind_RaceFirst  = 4 //// isRacer
  inline val Kind_RaceOne    = 5 //// isRacer

  inline def isMain(bits: Int): Boolean = bits == Kind_Main
  inline def isRoot(bits: Int): Boolean = bits <= Kind_Reentry
  inline def isRacer(bits: Int): Boolean = bits >= Kind_RaceAll
  inline def isReentry(bits: Int): Boolean = bits == Kind_Reentry
  inline def isExplicit(bits: Int): Boolean = bits == Kind_Explicit


  //// varyingBits:
  ////    completion   = 2 bits
  ////    cancellation = 2 bits


  inline val Completion_Pending    =   0 << Completion_Shift
  inline val Completion_Success    =   1 << Completion_Shift
  inline val Completion_Cancelled  =   2 << Completion_Shift
  inline val Completion_Failure    =   3 << Completion_Shift
  inline val Completion_Mask       = 0x3 << Completion_Shift
  inline val Completion_Shift      = 0

  inline val Cancellation_Signal  = 0x1 << Cancellation_Shift
  inline val Cancellation_Latch   = 0x2 << Cancellation_Shift
  inline val Cancellation_Mask    = 0x3 << Cancellation_Shift
  inline val Cancellation_Shift   = 2

  //@#@ temporary duplicates until compiler bug is fixed
  final val Completion_Success_bug: Int = Completion_Success
  final val Cancellation_Latch_Bug: Int = Cancellation_Latch

  def getCompletion(bits: Int): Int = bits & Completion_Mask
  def isPending(bits: Int): Boolean = getCompletion(bits) == Completion_Pending
  def isPendingAndNotCancelled(bits: Int): Boolean = (bits & (Completion_Mask | Cancellation_Signal)) == 0
  def isCancellationSignalled(bits: Int): Boolean = (bits & Cancellation_Signal) != 0
  def isCancellationUnlatched(bits: Int): Boolean = (bits & (Cancellation_Signal | Cancellation_Latch)) == Cancellation_Signal


  //// Warp

  inline val Warp_Pending   = Completion_Pending
  inline val Warp_Completed = Completion_Success
  inline val Warp_Cancelled = Cancellation_Signal
  inline val Warp_Shutdown  = Cancellation_Latch

  def isShutdown(bits: Int): Boolean = (bits & Warp_Shutdown) != 0
