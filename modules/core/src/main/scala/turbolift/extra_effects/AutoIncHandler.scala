package turbolift.extra_effects
import turbolift.abstraction.!!
import turbolift.std_effects.State


object AutoIncHandler:
  def apply[K, V, Fx <: AutoInc](fx: Fx, initial: Int = 0): fx.ThisIHandler[(Int, _)] =
    case object St extends State[Int]

    new fx.Proxy[St.type] with AutoIncSig:
      override def next: Int !@! ThisEffect = St.update(n => (n + 1, n))

    .toHandler
    .provideWith(St.handler(initial))
