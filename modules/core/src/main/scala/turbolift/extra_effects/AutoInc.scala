package turbolift.extra_effects
import turbolift.abstraction.{!!, Effect, Signature}


trait AutoIncSig extends Signature:
  def next: Int !@! ThisEffect


trait AutoInc extends Effect[AutoIncSig] with AutoIncSig:
  def next: Int !! this.type = impure(_.next)

  def handler: ThisIHandler[(Int, _)] = handler(0)
  def handler(initial: Int): ThisIHandler[(Int, _)] = AutoIncHandler.apply(this, initial)
