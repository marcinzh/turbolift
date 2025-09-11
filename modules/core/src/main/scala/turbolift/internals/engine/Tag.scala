package turbolift.internals.engine


//@#@ public bcoz inline problems
type Tag = Int


//@#@ public bcoz inline problems
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
  inline val LocalGetsEff   = 8
  inline val LocalPut       = 9
  inline val LocalModify    = 10
  inline val LocalUpdate    = 11
  inline val Sync           = 12

  //// Handled at middleLoop:
  inline val Intrinsic      = 13
  inline val Unwind         = 14

  //// Handled at outerLoop once, after fiber switch:
  inline val NotifyOnceVar       = 15
  inline val NotifyEffectfulVar  = 16
  inline val NotifyZipper        = 17
  inline val NotifyUnit          = 18
  inline val NotifyEither        = 19


  def toStr(tag: Tag) =
    tag match
      case FlatMap            => "FlatMap"
      case PureMap            => "PureMap"
      case MoreFlat           => "MoreFlat"
      case MorePure           => "MorePure"
      case Perform            => "Perform"
      case Pure               => "Pure"
      case Impure             => "Impure"
      case LocalGet           => "LocalGet"
      case LocalPut           => "LocalPut"
      case LocalUpdate        => "LocalUpdate"
      case Sync               => "Sync"

      case Intrinsic          => "Intrinsic"
      case Unwind             => "Unwind"

      case NotifyOnceVar      => "NotifyOnceVar"
      case NotifyEffectfulVar => "NotifyEffectfulVar"
      case NotifyZipper       => "NotifyZipper"
      case NotifyUnit         => "NotifyUnit"
      case NotifyEither       => "NotifyEither"
      case _                  => s"Tag($tag)"
