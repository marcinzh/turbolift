package turbolift.handlers
import turbolift.effects.{AutoInc, AutoIncSignature, State}
import turbolift.Extensions._


extension (fx: AutoInc)
  def autoIncHandler(initial: Int = 0): fx.ThisHandler[Identity, (_, Int), Any] =
    case object S extends State[Int]

    new fx.impl.Proxy[S.type] with AutoIncSignature:
      override def next: Int !@! ThisEffect = S.getModify(_ + 1)

    .toHandler
    .provideWith(S.handler(initial))
