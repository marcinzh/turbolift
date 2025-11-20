package turbolift.internals.engine


//@#@ public bcoz inline problems
object Bits:
  //// FiberImpl.theKind

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

  //// Waitee.theCompletion

  inline val Completion_Pending    = 0
  inline val Completion_Success    = 1
  inline val Completion_Cancelled  = 2
  inline val Completion_Failure    = 3

  //// Waitee.theCancellation

  inline val Cancellation_None       = 0
  inline val Cancellation_Signalled  = 1
  inline val Cancellation_Latched    = 2
