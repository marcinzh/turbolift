package turbolift.internals.effect
import turbolift.!!
import turbolift.effects.ChoiceSignature
import turbolift.internals.primitives.{ComputationCases => CC}


/** Virtual `Choice` effect instance.
 *
 *  Allows invoking operations of `Choice` effect,
 *  without the need of knowing its concrete instance.
 *  `AnyChoice` is designed to "match" the nearest (innermost) instance of `Choice` effect
 *  currently present in the effect stack.
 *
 *  This is needed for making the following mechanisms to work:
 *  - Guards in `for` comprehensions
 *  - `Alternative`-like composition, using binary `++!` operator
 */
private[turbolift] case object AnyChoice extends CanPerform[ChoiceSignature] with ChoiceSignature:
  override type ThisEffect >: ChoiceSignature

  override val empty: Nothing !! ThisEffect = perform(_.empty)
  override def choose[A](as: Iterable[A]): A !! ThisEffect = perform(_.choose(as))
  override def choosePar[A](as: Iterable[A]): A !! ThisEffect = perform(_.choosePar(as))

  def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = choose(Vector(lhs, !!.impureEff(rhs))).flatten
  def plusPar[A, U <: ThisEffect](lhs: A !! U, rhs: A !! U): A !! U = choosePar(Vector(lhs, rhs)).flatten
