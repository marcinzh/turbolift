package turbolift.extra_effects.default_handlers
import turbolift.std_effects.State
import turbolift.extra_effects.{AutoInc, AutoIncSig}


extension (fx: AutoInc)
  private[extra_effects] def autoIncHandler(initial: Int = 0): fx.ThisHandler.Free[(_, Int)] =
    case object S extends State[Int]

    new fx.Proxy[S.type] with AutoIncSig:
      override def next: Int !@! ThisEffect = S.getModify(_ + 1)

    .toHandler
    .provideWith(S.handler(initial))
