package turbolift.internals.engine.concurrent
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{Zipper, Outcome, Cause, Snap}
import turbolift.internals.engine.stacked.{Stack, OpCascaded}
import turbolift.internals.engine.Misc._
import ZipperCases._


private[engine] sealed abstract class ZipperImpl extends Zipper.Unsealed:
  final override def run: Any !! Nothing =
    this match
      case Functor(payload, stack) => OpCascaded.restart(stack, payload)
      case Compute(comp) => comp
      case Cancelled => IO.cancel
      case Failure(c) => IO.fail(c)


  final override def outcome: Outcome[Unit] =
    this match
      case Cancelled => Outcome.Cancelled
      case Failure(c) => Outcome.Failure(c)
      case _ => Outcome.unit


  final override def get(using Any <:< Nothing): Any =
    this match
      case Functor(pay, _) => pay
      case _ => impossible


  final override def getIO(using IO <:< Nothing): Outcome[Any] =
    this match
      case Functor(pay, _) => Outcome.Success(pay)
      case Cancelled => Outcome.Cancelled
      case Failure(c) => Outcome.Failure(c)
      case _ => impossible


  final override def doHandleIO[V]: Outcome[Zipper[Any, V]] =
    this match
      case Cancelled => Outcome.Cancelled
      case Failure(c) => Outcome.Failure(c)
      case _ => Outcome.Success(cast[Any, V])


  final override def doZip(that: Zipper.Untyped, f: (Any, Any) => Any): Zipper.Untyped =
    (this, that) match
      case (Failure(c), Failure(d)) => Failure(c & d)
      case (Failure(_), _) => this
      case (_, Failure(_)) => that
      case (Cancelled, _) => Cancelled
      case (_, Cancelled) => Cancelled
      case (Functor(a, s), Functor(b, s2)) if s.hasSamePromptsAs(s2) => Functor(OpCascaded.zip(s, a, b, f), s)
      case _ => Compute(this.run.zipWithPar(that.run)(f))


private object ZipperCases:
  case object Cancelled extends ZipperImpl
  final case class Failure(cause: Cause) extends ZipperImpl
  sealed abstract class Success extends ZipperImpl
  //@#@OPTY instead of stack, just array of interpreters would suffice, but then OpCascaded would need to be rewritten
  final case class Functor(val payload: Any, val stack: Stack) extends Success
  final case class Compute(val comp: Any !! Nothing) extends Success


private object ZipperImpl:
  def make(stack: Stack | Null, payload: Any, completion: Int): ZipperImpl =
    completion match
      case Bits.Completion_Success => Functor(payload, stack.nn.getJoin)
      case Bits.Completion_Failure => Failure(payload.asInstanceOf[Cause])
      case Bits.Completion_Cancelled => Cancelled
