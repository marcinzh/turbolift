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
  inline val Local               = 9
  inline val Abort               = 10
  inline val Step_Capture        = 11
  inline val Step_Bridge         = 12
  inline val ZipPar              = 13
  inline val ZipSeq              = 14
  inline val OrPar               = 15
  inline val OrSeq               = 16
  inline val Step_ZipSeqLeft     = 17
  inline val Step_ZipSeqRight    = 18
  inline val EnvAsk              = 19
  inline val EnvMod              = 20
  inline val DoSnap              = 21
  inline val Unsnap              = 22
  inline val Yield               = 23
  inline val Handle              = 24
  inline val Step_Push           = 25
  inline val Step_Pop            = 26
  inline val Step_Unwind         = 27


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
      case Local               => "Local"
      case Abort               => "Abort"
      case Step_Capture        => "Step_Capture"
      case Step_Bridge         => "Step_Bridge"
      case ZipPar              => "ZipPar"
      case ZipSeq              => "ZipSeq"
      case OrPar               => "OrPar"
      case OrSeq               => "OrSeq"
      case Step_ZipSeqLeft     => "Step_ZipSeqLeft"
      case Step_ZipSeqRight    => "Step_ZipSeqRight"
      case EnvAsk              => "EnvAsk"
      case EnvMod              => "EnvMod"
      case DoSnap              => "DoSnap"
      case Unsnap              => "Unsnap"
      case Yield               => "Yield"
      case Handle              => "Handle"
      case Step_Push           => "Step_Push"
      case Step_Pop            => "Step_Pop"
      case Step_Unwind         => "Step_Unwind"
      case _                   => s"Tag($tag)"
