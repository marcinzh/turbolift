package turbolift.std_effects
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, AlternativeSig}
import turbolift.std_handlers.DefaultChoiceHandler


trait ChoiceSig[U] extends AlternativeSig[U] {
  def each[A](as: Iterable[A]): A !! U
}


trait Choice extends Effect.Alternative[ChoiceSig] {
  def each[A](as: Iterable[A]) = encodeFO(_.each(as))
  
  def fromEach[A](as: A*) = each(as.toVector)

  val handler = DefaultChoiceHandler(this)
}
