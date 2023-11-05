package turbolift.extra_effects.default_handlers
import turbolift.effects.State
import turbolift.extra_effects.{AutoInc, AutoIncSignature}


extension (fx: AutoInc)
  private[extra_effects] def autoIncHandler(initial: Int = 0): fx.ThisHandler.Free[(_, Int)] =
    case object S extends State[Int]

    new fx.impl.Proxy[S.type] with AutoIncSignature:
      override def next: Int !@! ThisEffect = _ => S.getModify(_ + 1)

    .toHandler
    .provideWith(S.handler(initial))
