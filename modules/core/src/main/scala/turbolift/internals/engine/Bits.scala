package turbolift.internals.engine


private[engine] object Bits:
  
  //// Common:

  final val Child_None  = 0
  final val Child_Left  = 1
  final val Child_Right = 2
  final val Child_Both  = 3
  final val Child_Mask  = 3

  //// constantBits:  self-child | tree

  final val Tree_Root = 0
  final val Tree_Zip  = 1
  final val Tree_Race = 2
  final val Tree_Mask = 0xf

  final val Self_Shift = 4
  final val Self_Left  = Child_Left  << Self_Shift
  final val Self_Right = Child_Right << Self_Shift
  // final val Self_ShiftedMask = Child_Mask << Self_Shift

  final val Zip_Left  = Tree_Zip | Self_Left
  final val Zip_Right = Tree_Zip | Self_Right

  //// varyingBits:  flags | awaited-child | successful-child

  final val Cancelled = 0x100
  final val Winner    = 0x200

  final val Awaited_Shift = 4
  final val Awaited_None  = Child_None  << Awaited_Shift 
  final val Awaited_Left  = Child_Left  << Awaited_Shift 
  final val Awaited_Right = Child_Right << Awaited_Shift 
  final val Awaited_Both  = Child_Both  << Awaited_Shift 
  final val Awaited_ShiftedMask = Child_Mask << Awaited_Shift 
