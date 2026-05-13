package spot.internals
import cats.data.State
import cats.effect.kernel.Ref
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.AtomicVar


final class SpotRef[S, U <: IO] private (underlying: AtomicVar[S]) extends Ref[!![_, U], S]:
  //// Members declared in cats.effect.kernel.RefSource
  
  override def get: S !! U = underlying.get

  //// Members declared in cats.effect.kernel.RefSink

  override def set(a: S): Unit !! U = underlying.put(a)
  
  //// Members declared in cats.effect.kernel.Ref

  override def update(f: S => S): Unit !! U = underlying.modify(f)

  override def modify[A](f: S => (S, A)): A !! U = underlying.update(f.andThen(_.swap))

  override def modifyState[A](s: State[S, A]): A !! U = modify(s.run.andThen(_.value))

  override def tryUpdate(f: S => S): Boolean !! U = underlying.tryModify(f)

  override def tryModify[A](f: S => (S, A)): Option[A] !! U = underlying.tryUpdate(f.andThen(_.swap))

  override def tryModifyState[A](s: State[S, A]): Option[A] !! U = tryModify(s.run.andThen(_.value))

  override def access: (S, S => Boolean !! U) !! U =
    !!.impure:
      val s = underlying.unsafeGet
      val f = (s2: S) => !!.impure(underlying.unsafeCompareAndSet(s, s2))
      (s, f)
  

object SpotRef:
  def create[S, U <: IO](initial: S): SpotRef[S, U] !! U = AtomicVar(initial).map(new SpotRef(_))
