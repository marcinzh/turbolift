package turbolift.internals.engine


private[internals] sealed abstract class Halt extends Halt.Loop


private[internals] object Halt:
  //// Public for Executor
  case object Yield extends Halt
  case object Retire extends Halt

  //// Result of `innerLoop`
  private[engine] sealed abstract class Loop
  private[engine] case object Become extends Loop

  inline def ThreadDisowned = Retire
