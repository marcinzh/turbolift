package turbolift.internals.engine


private[internals] sealed abstract class Halt extends Halt.Loop1st


private[internals] object Halt:
  //// Public for Executor
  case object Yield extends Halt
  case object Retire extends Halt

  //// Result of `innerLoop`
  private[engine] sealed abstract class Loop1st extends Loop2nd
  private[engine] sealed abstract class Loop2nd
  private[engine] case object Become extends Loop1st
  private[engine] case object Bounce extends Loop2nd
