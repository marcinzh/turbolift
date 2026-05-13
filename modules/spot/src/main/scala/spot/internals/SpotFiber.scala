package spot.internals
import cats.effect.kernel.{Fiber => CatsFiber, Outcome => CatsOutcome}
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.Fiber
import turbolift.data.Outcome


final case class SpotFiber[A, U <: IO](underlying: Fiber[A, U]) extends CatsFiber[!![_, U], Throwable, A]:
  override def cancel: Unit !! U = underlying.cancel
  
  override def join: CatsOutcome[!![_, U], Throwable, A] !! U =
    underlying.await.map: z =>
      z.handleIO match
        case Outcome.Success(z) => CatsOutcome.Succeeded(z.run)
        case Outcome.Cancelled => CatsOutcome.Canceled()
        case Outcome.Failure(c) => CatsOutcome.Errored(c.last.toThrowable)


object SpotFiber:
  def start[A, U <: IO](comp: A !! U): SpotFiber[A, U] !! IO =
    Fiber.fork(comp).map(SpotFiber(_)).downCast[IO]
