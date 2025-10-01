package turbolift.internals.engine


//@#@ public bcoz inline problems
type Halt = Int

//@#@ public bcoz inline problems
object Halt:
  inline val ContinueNoTick = 0
  inline val Continue       = 1
  inline val Cancel         = 2
  inline val Reset          = 3
  inline val Become         = 4
  inline val Yield          = 5
  inline val Retire         = 8
