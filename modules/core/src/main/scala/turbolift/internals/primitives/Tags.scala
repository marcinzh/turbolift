package turbolift.internals.primitives


private[turbolift] object Tags:
  inline val MapFlat             = 0
  inline val MapPure             = 1
  inline val Step_MoreFlat       = 2
  inline val Step_MorePure       = 3
  inline val Perform             = 4
  inline val Pure                = 5
  inline val Impure              = 6
  inline val LocalGet            = 7
  inline val LocalPut            = 8
  inline val LocalUpdate         = 9
  inline val Delimit             = 10
  inline val Abort               = 11
  inline val Resume              = 12
  inline val Capture             = 13
  inline val Step_Bridge         = 14
  inline val ZipPar              = 15
  inline val ZipSeq              = 16
  inline val OrPar               = 17
  inline val OrSeq               = 18
  inline val Step_ZipSeqLeft     = 19
  inline val Step_ZipSeqRight    = 20
  inline val Handle              = 21
  inline val Step_Push           = 22
  inline val Step_Unwind         = 23
  inline val DoIO                = 24
  inline val DoTry               = 25
  inline val DoSnap              = 26
  inline val Unsnap              = 27
  inline val EnvAsk              = 28
  inline val EnvMod              = 29
  inline val Yield               = 30


  def toStr(tag: Int) =
    tag match
      case MapFlat             => "MapFlat"
      case MapPure             => "MapPure"
      case Step_MoreFlat       => "Step_MoreFlat"
      case Step_MorePure       => "Step_MorePure"
      case Perform             => "Perform"
      case Pure                => "Pure"
      case Impure              => "Impure"
      case LocalGet            => "LocalGet"
      case LocalPut            => "LocalPut"
      case LocalUpdate         => "LocalUpdate"
      case Delimit             => "Delimit"
      case Abort               => "Abort"
      case Resume              => "Resume"
      case Capture             => "Capture"
      case Step_Bridge         => "Step_Bridge"
      case ZipPar              => "ZipPar"
      case ZipSeq              => "ZipSeq"
      case OrPar               => "OrPar"
      case OrSeq               => "OrSeq"
      case Step_ZipSeqLeft     => "Step_ZipSeqLeft"
      case Step_ZipSeqRight    => "Step_ZipSeqRight"
      case Handle              => "Handle"
      case Step_Push           => "Step_Push"
      case Step_Unwind         => "Step_Unwind"
      case DoIO                => "DoIO"
      case DoTry               => "DoTry"
      case DoSnap              => "DoSnap"
      case Unsnap              => "Unsnap"
      case EnvAsk              => "EnvAsk"
      case EnvMod              => "EnvMod"
      case Yield               => "Yield"
      case _                   => s"Tag($tag)"
