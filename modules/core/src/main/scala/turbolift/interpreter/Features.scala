package turbolift.interpreter


private[turbolift] opaque type Features = Byte

private[turbolift] object Features extends Features_opaque:
  private[interpreter] inline def wrap(that: Byte): Features = that

  private[interpreter] def cond(x: Features, y: Boolean): Features = if y then x else Empty

  extension (thiz: Features)
    inline def unwrap: Byte = thiz

    inline def isStateful: Boolean     = (thiz & STATEFUL) != 0
    inline def isChoice: Boolean       = (thiz & CHOICE) != 0
    inline def isSequential: Boolean   = (thiz & SEQUENTIAL) != 0
    inline def hasZip: Boolean         = (thiz & ZIP) != 0
    inline def hasForkJoin: Boolean    = (thiz & FORKJOIN) != 0
    inline def hasRestart: Boolean     = (thiz & RESTART) != 0
    inline def isIo: Boolean           = (thiz & IO) != 0

    inline def isParallel: Boolean = !isSequential
    inline def isStateless: Boolean = !isStateful

  private inline val STATEFUL       = 0x1
  private inline val SEQUENTIAL     = 0x2
  private inline val CHOICE         = 0x4
  private inline val ZIP            = 0x8
  private inline val FORKJOIN       = 0x10
  private inline val RESTART        = 0x20
  //--------------------------------------
  private inline val MASK           = 0x3F
  private inline val IO             = 0x40
  //@#@TODO hints

  def Empty: Features = 0
  private[interpreter] def Stateful: Features    = STATEFUL
  private[interpreter] def Sequential: Features  = SEQUENTIAL
  private[interpreter] def Choice: Features      = CHOICE
  private[interpreter] def Zip: Features         = ZIP
  private[interpreter] def ForkJoin: Features    = FORKJOIN
  private[interpreter] def Restart: Features     = RESTART
  private[interpreter] def Io: Features          = IO
  private[interpreter] def Mask: Features        = MASK
