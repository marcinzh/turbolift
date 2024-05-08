package turbolift.internals.primitives


private[turbolift] object Tags:
  inline val MapFlat             = 0
  inline val MapPure             = 1
  inline val Step_MoreFlat       = 2
  inline val Step_MorePure       = 3
  inline val Perform             = 4
  inline val Pure                = 5
  inline val Impure              = 6
  inline val Resume              = 7
  inline val Escape              = 8
  inline val Delimit             = 9
  inline val Abort               = 10
  inline val Step_Capture        = 11
  inline val Step_Bridge         = 12
  inline val ZipPar              = 13
  inline val ZipSeq              = 14
  inline val OrPar               = 15
  inline val OrSeq               = 16
  inline val Step_ZipSeqLeft     = 17
  inline val Step_ZipSeqRight    = 18
  inline val Handle              = 19
  inline val Step_Push           = 20
  inline val Step_Unwind         = 21
  inline val DoIO                = 22
  inline val DoTry               = 23
  inline val DoSnap              = 24
  inline val Unsnap              = 25
  inline val EnvAsk              = 26
  inline val EnvMod              = 27
  inline val Yield               = 28


  def toStr(tag: Int) =
    tag match
      case MapFlat             => "MapFlat"
      case MapPure             => "MapPure"
      case Step_MoreFlat       => "Step_MoreFlat"
      case Step_MorePure       => "Step_MorePure"
      case Perform             => "Perform"
      case Pure                => "Pure"
      case Impure              => "Impure"
      case Resume              => "Resume"
      case Escape              => "Escape"
      case Delimit             => "Delimit"
      case Abort               => "Abort"
      case Step_Capture        => "Step_Capture"
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
