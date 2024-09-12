package turbolift.internals.engine


type Tag = Int

object Tag:
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
  inline val Intrinsic      = 11
  inline val Unwind         = 12
  inline val Bounce         = 13

  inline val NotifyOnceVar  = 14
  inline val NotifyZipper   = 15
  inline val NotifyUnit     = 16

  inline val Become         = 17
  inline val Yield          = 18
  inline val Retire         = 19

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
      case Bounce         => "Bounce"

      case NotifyOnceVar  => "NotifyOnceVar"
      case NotifyZipper   => "NotifyZipper"
      case NotifyUnit     => "NotifyUnit"

      case Become         => "Become"
      case Yield          => "Yield"
      case Retire         => "Retire"
      case _              => s"Tag($tag)"
