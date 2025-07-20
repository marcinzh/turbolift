package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.Extensions._


trait AutoIncSignature extends Signature:
  def next: Int !! ThisEffect


trait AutoInc extends Effect[AutoIncSignature] with AutoIncSignature:
  def next: Int !! this.type = perform(_.next)

  /** Default handler for this effect. */
  def handler: ThisHandler[Identity, (_, Int), Any] = handler(0)
  
  /** Predefined handler for this effect. */
  def handler(initial: Int): ThisHandler[Identity, (_, Int), Any] =
    case object S extends StateEffect[Int]
    new impl.Proxy[S.type] with AutoIncSignature:
      override def next: Int !! ThisEffect = S.getModify(_ + 1)
    .toHandler
    .provideWith(S.handlers.local(initial))
