package spot.internals
import cats.effect.kernel.Deferred
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.OnceVar


final class SpotDeferred[A, U <: IO] private (underlying: OnceVar[A]) extends Deferred[!![_, U], A]:
  //// Members declared in cats.effect.kernel.DeferredSource

  override def get: A !! U = underlying.get

  override def tryGet: Option[A] !! U = underlying.tryGet

  //// Members declared in cats.effect.kernel.DeferredSink

  override def complete(a: A): Boolean !! U = underlying.tryPut(a)
  

object SpotDeferred:
  def create[A, U <: IO](): SpotDeferred[A, U] !! U = OnceVar.create[A].map(new SpotDeferred(_))
