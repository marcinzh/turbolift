package turbolift.internals.effect
import scala.util.Try
import turbolift.!!
import turbolift.std_effects.ChoiceSig


private[turbolift] case object AnyChoice extends CanPerform[ChoiceSig] with ChoiceSig:
  override type ThisEffect >: ChoiceSig

  override val fail: Nothing !! ThisEffect = perform(_.fail)
  override def orElse[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = perform(_.orElse(lhs, rhs))
  override def choose[A](as: Iterable[A]): A !! ThisEffect = perform(_.choose(as))
