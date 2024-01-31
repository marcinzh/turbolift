package turbolift.handlers
import turbolift.effects.{AutoInc, AutoIncSignature}
import turbolift.effects.State


extension (fx: AutoInc)
  def autoIncHandler(initial: Int = 0): fx.ThisHandler.FromId.Free[(_, Int)] =
    case object S extends State[Int]

    new fx.impl.Proxy[S.type] with AutoIncSignature:
      override def next: Int !@! ThisEffect = S.getModify(_ + 1)

    .toHandler
    .provideWith(S.handler(initial))
