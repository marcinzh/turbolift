package turbolift.internals.engine


//@#@ public bcoz inline problems
type Halt = Int

//@#@ public bcoz inline problems
object Halt:
  inline val ContinueNoTick = 0
  inline val Continue       = 1
  inline val Reset          = 2
  inline val Become         = 3
  inline val Yield          = 4
  inline val Retire         = 5
