package turbolift.extra_effects.default_handlers
import turbolift.std_effects.State
import turbolift.extra_effects.{AutoInc, AutoIncSig}


private[extra_effects] object AutoIncHandler:
  def apply[K, V, Fx <: AutoInc](fx: Fx, initial: Int = 0): fx.ThisHandler.Free[(_, Int)] =
    case object St extends State[Int]

    new fx.Proxy[St.type] with AutoIncSig:
      override def next: Int !@! ThisEffect = St.getModify(_ + 1)

    .toHandler
    .provideWith(St.handler(initial))
