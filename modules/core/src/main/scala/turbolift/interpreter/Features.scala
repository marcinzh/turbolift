package turbolift.interpreter


private[turbolift] opaque type Features = Byte

private[turbolift] object Features extends Features_opaque:
  inline def wrap(that: Byte): Features = that

  private[interpreter] def cond(x: Features, y: Boolean): Features = if y then x else Empty

  extension (thiz: Features)
    inline def unwrap: Byte = thiz

    def isStateful: Boolean     = (thiz & STATEFUL) != 0
    def isChoice: Boolean       = (thiz & CHOICE) != 0
    def isSequential: Boolean   = (thiz & SEQUENTIAL) != 0
    def hasZip: Boolean         = (thiz & ZIP) != 0
    def hasForkJoin: Boolean    = (thiz & FORKJOIN) != 0
    def hasRestart: Boolean     = (thiz & RESTART) != 0
    def isIo: Boolean           = (thiz & IO) != 0

    def isParallel: Boolean = !isSequential
    def isStateless: Boolean = !isStateful

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

  def Empty: Features       = 0
  def Stateful: Features    = STATEFUL
  def Sequential: Features  = SEQUENTIAL
  def Choice: Features      = CHOICE
  def Zip: Features         = ZIP
  def ForkJoin: Features    = FORKJOIN
  def Restart: Features     = RESTART
  def Io: Features          = IO
  def Mask: Features        = MASK
