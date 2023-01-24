package turbolift.internals.primitives


private[turbolift] object Tags:
  final val Invalid                   = -1
  final val MapFlat                   = 0
  final val MapPure                   = 1
  final val Perform                   = 2
  final val Step_Hop                  = 3
  final val Step_MoreFlat             = 4
  final val Step_MorePure             = 5
  final val Pure                      = 6
  final val Impure                    = 7
  final val ZipWithPar                = 8
  final val Step_ZipLeft              = 9
  final val Step_ZipRight             = 10
  final val Escape                    = 11
  final val Resume                    = 12
  final val Local                     = 13
  final val Step_Restore              = 14
  final val Step_Capture              = 15
  final val Step_Done                 = 16
  final val Step_Abort                = 17
  final val Handle                    = 18
  final val History_Empty             = 19
  final val History_Proxied           = 20
  final val History_Configured        = 21
  final val ConfigAsk                 = 22
  final val ConfigMod                 = 23



  def toStr(tag: Int) =
    tag match
      case Invalid => "Invalid"
      case Step_MoreFlat => "Step_MoreFlat"
      case Step_MorePure => "Step_MorePure"
      case MapFlat => "MapFlat"
      case MapPure => "MapPure"
      case Pure => "Pure"
      case Perform => "Perform"
      case ZipWithPar => "ZipWithPar"
      case Step_ZipLeft => "Step_ZipLeft"
      case Step_ZipRight => "Step_ZipRight"
      case Impure => "Impure"
      case Resume => "Resume"
      case Local => "Local"
      case Escape => "Escape"
      case Handle => "Handle"
      case ConfigAsk => "ConfigAsk"
      case ConfigMod => "ConfigMod"
      case Step_Restore => "Step_Restore"
      case Step_Capture => "Step_Capture"
      case Step_Done => "Step_Done"
      case Step_Abort => "Step_Abort"
      case History_Empty => "History_Empty"
      case History_Proxied => "History_Proxied"
      case History_Configured => "History_Configured"
      case _ => "Tag???"
