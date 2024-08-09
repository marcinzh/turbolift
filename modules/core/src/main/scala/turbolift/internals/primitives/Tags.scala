package turbolift.internals.primitives


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
  inline val Delimit               = 10
  inline val Abort                 = 11
  inline val Resume                = 12
  inline val Capture               = 13
  inline val Step_Bridge           = 14
  inline val ZipPar                = 15
  inline val OrPar                 = 16
  inline val OrSeq                 = 17
  inline val Handle                = 18
  inline val Step_Push             = 19
  inline val Step_Unwind           = 20
  inline val DoIO                  = 21
  inline val DoSnap                = 22
  inline val Unsnap                = 23
  inline val EnvAsk                = 24
  inline val EnvMod                = 25
  inline val AwaitOnceVar          = 26
  inline val NotifyOnceVar         = 27
  inline val ForkFiber             = 28
  inline val AwaitFiber            = 29
  inline val NotifyFiber           = 30
  inline val NotifyFiberVoid       = 31
  inline val CurrentFiber          = 32
  inline val SpawnWarp             = 33
  inline val AwaitWarp             = 34
  inline val Blocking              = 35
  inline val Sleep                 = 36
  inline val NotifyBlocker         = 37
  inline val Suppress              = 38
  inline val ExecOn                = 39
  inline val Yield                 = 40


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
      case Delimit               => "Delimit"
      case Abort                 => "Abort"
      case Resume                => "Resume"
      case Capture               => "Capture"
      case Step_Bridge           => "Step_Bridge"
      case ZipPar                => "ZipPar"
      case OrPar                 => "OrPar"
      case OrSeq                 => "OrSeq"
      case Handle                => "Handle"
      case Step_Push             => "Step_Push"
      case Step_Unwind           => "Step_Unwind"
      case DoIO                  => "DoIO"
      case DoSnap                => "DoSnap"
      case Unsnap                => "Unsnap"
      case EnvAsk                => "EnvAsk"
      case EnvMod                => "EnvMod"
      case AwaitOnceVar          => "AwaitOnceVar"
      case NotifyOnceVar         => "NotifyOnceVar"
      case ForkFiber             => "ForkFiber"
      case AwaitFiber            => "AwaitFiber"
      case NotifyFiber           => "NotifyFiber"
      case NotifyFiberVoid       => "NotifyFiberVoid"
      case CurrentFiber          => "CurrentFiber"
      case SpawnWarp             => "SpawnWarp"
      case AwaitWarp             => "AwaitWarp"
      case Blocking              => "Blocking"
      case Sleep                 => "Sleep"
      case NotifyBlocker         => "NotifyBlocker"
      case Suppress              => "Suppress"
      case ExecOn                => "ExecOn"
      case Yield                 => "Yield"
      case _                     => s"Tag($tag)"
