package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.handlers.autoIncHandler


trait AutoIncSignature extends Signature:
  def next: Int !@! ThisEffect


trait AutoInc extends Effect[AutoIncSignature] with AutoIncSignature:
  def next: Int !! this.type = perform(_.next)

  /** Default handler for this effect. */
  def handler: ThisHandler.FromId.Free[(_, Int)] = handler(0)
  
  /** Predefined handler for this effect. */
  def handler(initial: Int): ThisHandler.FromId.Free[(_, Int)] = this.autoIncHandler(initial)
