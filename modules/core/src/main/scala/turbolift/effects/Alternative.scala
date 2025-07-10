package turbolift.effects
import turbolift.!!
import turbolift.{ComputationCases => CC}


/** Virtual `Choice` effect instance.
 *
 *  Allows invoking operations of `Choice` effect,
 *  without the need of knowing its concrete instance.
 *  `Alternative` is designed to "match" the nearest (innermost) instance of `Choice` effect
 *  currently present in the effect stack.
 *
 *  This is needed for making the following mechanisms to work:
 *  - Guards in `for` comprehensions use `Alternative.empty`
 *  - Binary operators `+!` and `++!` (similar to `<+>` from Cats) use `plus` and `plusPar`.
 */
case object Alternative extends ChoiceSignature:
  override type ThisEffect >: ChoiceSignature

  override val empty: Nothing !! ThisEffect = perform(_.empty)
  override def choose[A](as: Iterable[A]): A !! ThisEffect = perform(_.choose(as))
  override def choosePar[A](as: Iterable[A]): A !! ThisEffect = perform(_.choosePar(as))

  def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = choose(Vector(lhs, !!.impureEff(rhs))).flatten
  def plusPar[A, U <: ThisEffect](lhs: A !! U, rhs: A !! U): A !! U = choosePar(Vector(lhs, rhs)).flatten

  private final inline def perform[A, U <: ThisEffect](inline f: (ChoiceSignature { type ThisEffect = U }) => A !! U): A !! U =
    CC.perform[A, U, ChoiceSignature { type ThisEffect = U }](this, f)
