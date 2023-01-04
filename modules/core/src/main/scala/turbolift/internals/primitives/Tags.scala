package turbolift.internals.primitives


private[turbolift] object Tags:
  final val Invalid                   = -1
  final val MapFlat                   = 0
  final val MapPure                   = 1
  final val Perform                   = 2
  final val Step_MoreFlat             = 3
  final val Step_MorePure             = 4
  final val Pure                      = 5 
  final val Impure                    = 6
  final val ZipWithPar                = 7
  final val Step_Done                 = 8
  final val Step_Abort                = 9
  final val Handle                   = 10
  final val Resume                    = 11
  final val Local                     = 12 
  final val Step_Restore              = 13 
  final val Step_Capture              = 14
  final val History_Empty             = 15
  final val History_Proxied           = 16
  final val History_Configured        = 17
  final val ConfigAsk                 = 18
  final val ConfigMod                 = 19



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
      case Impure => "Impure"
      case Resume => "Resume"
      case Local => "Local"
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
