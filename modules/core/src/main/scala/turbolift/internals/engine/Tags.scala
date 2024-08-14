package turbolift.internals.engine


//@#@TEMP public bcoz inline bug
// private[turbolift] object Tags:
object Tags:
  inline val MapFlat               = 0
  inline val MapPure               = 1
  inline val Step_MoreFlat         = 2
  inline val Step_MorePure         = 3
  inline val Perform               = 4
  inline val Pure                  = 5
  inline val Impure                = 6
  inline val LocalGet              = 7
  inline val LocalPut              = 8
  inline val LocalUpdate           = 9
  inline val Sync                  = 10

  inline val Intristic             = 11
  inline val NotifyOnceVar         = 12
  inline val NotifyFiber           = 13
  inline val NotifyFiberVoid       = 14
  inline val NotifyEither          = 15
  inline val NotifyBlocker         = 16
  inline val Step_Push             = 17
  inline val Step_Bridge           = 18
  inline val Step_Unwind           = 19

  def toStr(tag: Int) =
    tag match
      case MapFlat               => "MapFlat"
      case MapPure               => "MapPure"
      case Step_MoreFlat         => "Step_MoreFlat"
      case Step_MorePure         => "Step_MorePure"
      case Perform               => "Perform"
      case Pure                  => "Pure"
      case Impure                => "Impure"
      case LocalGet              => "LocalGet"
      case LocalPut              => "LocalPut"
      case LocalUpdate           => "LocalUpdate"
      case Sync                  => "Sync"

      case Intristic             => "Intristic"
      case NotifyOnceVar         => "NotifyOnceVar"
      case NotifyFiber           => "NotifyFiber"
      case NotifyFiberVoid       => "NotifyFiberVoid"
      case NotifyEither          => "NotifyEither"
      case NotifyBlocker         => "NotifyBlocker"
      case Step_Push             => "Step_Push"
      case Step_Bridge           => "Step_Bridge"
      case Step_Unwind           => "Step_Unwind"
      case _                     => s"Tag($tag)"
