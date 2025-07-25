package turbolift.misc
import org.specs2.mutable._
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.effects.{IO, ErrorEffect}
import turbolift.data.{Outcome, Cause}
import turbolift.io.AtomicVar
import Auxx._


class AbortTest extends Specification:
  sequential

  case object Except extends RuntimeException

  case object Error extends ErrorEffect[String]
  type Error = Error.type

  sealed trait PreciousSig extends Signature:
    def add(comp: Unit !! IO): Unit !! ThisEffect

  type Precious = Precious.type
  case object Precious extends Effect[PreciousSig] with PreciousSig:
    override def add(comp: Unit !! IO): Unit !! ThisEffect = perform(_.add(comp))

    def handler =
      new impl.Stateful[Identity, Identity, IO] with impl.Sequential with PreciousSig:
        //@#@TODO compiler `Locl`
        override type Local = Unit !! IO
        override def onInitial = !!.unit.pure_!!
        override def onReturn(x: Unknown, s: Local) = s.as(x)
        override def onAbort(s: Local) = s
        override def add(comp: Unit !! IO) = Local.modify(comp &&! _)
      .toHandler

  enum Mode:
    case Nop
    case Cancel
    case Except
    case Error

  def prog(mode: Mode) =
    (for
      v <- AtomicVar(0)
      a <-
        IO.snap:
          (for
            _ <- Precious.add(v.event(1))
            _ <- Precious.add(v.event(2))
            _ <-
              mode match
                case Mode.Nop => !!.unit
                case Mode.Cancel => IO.cancel
                case Mode.Except => IO.raise(Except)
                case Mode.Error => Error.raise("OMG")
            _ <- Precious.add(v.event(3))
          yield ())
          .handleWith(Precious.handler)
          .handleWith(Error.handler)
        .map(_.toOutcome)
      n <- v.get
    yield (a, n))
    .runIO

  "Nop"    >>{ prog(Mode.Nop)    === Outcome.Success((Outcome.Success(Right(())), 321)) }
  "Error"  >>{ prog(Mode.Error)  === Outcome.Success((Outcome.Success(Left("OMG")), 21)) }
  "Except" >>{ prog(Mode.Except) === Outcome.Success((Outcome.Failure(Cause.Thrown(Except)), 21)) }
  "Cancel" >>{ prog(Mode.Cancel) === Outcome.Success((Outcome.Cancelled, 21)) }
