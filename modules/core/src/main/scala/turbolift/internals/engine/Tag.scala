package turbolift.internals.engine


type Tag = Int

object Tag:
  inline val TickReset      = 0x40

  //// Handled at innerLoop:
  inline val FlatMap        = 0
  inline val PureMap        = 1
  inline val MoreFlat       = 2
  inline val MorePure       = 3
  inline val Perform        = 4
  inline val Pure           = 5
  inline val Impure         = 6
  inline val LocalGet       = 7
  inline val LocalPut       = 8
  inline val LocalUpdate    = 9
  inline val Sync           = 10

  //// Handled at middleLoop:
  inline val Intrinsic      = 11
  inline val Unwind         = 12

  //// Handled at outerLoop: (`Become` MUST be first in this group)
  inline val Become         = 13
  inline val Yield          = 14
  inline val Retire         = 15

  //// Handled at outerLoop once, after fiber switch:
  inline val NotifyOnceVar  = 16
  inline val NotifyZipper   = 17
  inline val NotifyUnit     = 18


  def toStr(tag: Tag) =
    tag match
      case FlatMap        => "FlatMap"
      case PureMap        => "PureMap"
      case MoreFlat       => "MoreFlat"
      case MorePure       => "MorePure"
      case Perform        => "Perform"
      case Pure           => "Pure"
      case Impure         => "Impure"
      case LocalGet       => "LocalGet"
      case LocalPut       => "LocalPut"
      case LocalUpdate    => "LocalUpdate"
      case Sync           => "Sync"

      case Intrinsic      => "Intrinsic"
      case Unwind         => "Unwind"

      case Become         => "Become"
      case Yield          => "Yield"
      case Retire         => "Retire"

      case NotifyOnceVar  => "NotifyOnceVar"
      case NotifyZipper   => "NotifyZipper"
      case NotifyUnit     => "NotifyUnit"
      case _              => s"Tag($tag)"
