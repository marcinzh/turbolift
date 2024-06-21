package turbolift.internals.engine


private[internals] sealed abstract class Halt extends Halt.Loop


private[internals] object Halt:
  //// Public for Executor
  final case class Yield(fiber: FiberImpl) extends Halt
  final case class Retire(reentry: Boolean) extends Halt

  //// Result of `innerLoop`
  private[engine] sealed abstract class Loop
  private[engine] case object Become extends Loop

  inline def ThreadDisowned = RetireFalse

  private[engine] def retire(reentry: Boolean): Halt = if reentry then RetireTrue else RetireFalse
  private val RetireTrue = Retire(true)
  private val RetireFalse = Retire(false)
