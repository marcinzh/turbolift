package turbolift.extra_effects
import turbolift.abstraction.!!
import turbolift.std_effects.State


object AutoIncHandler {
  def apply[K, V, Fx <: AutoInc](fx: Fx, initial: Int = 0): fx.ThisIHandler[(Int, *)] = {
    case object St extends State[Int]

    new fx.Dependent[St.type] {
      override def interpret[U <: St.type] = new AutoIncSig[U] {
        override def next: Int !! U = St.update(n => (n + 1, n))
      }
    }
    .toHandler
    .provideWith(St.handler(initial))
  }
}
