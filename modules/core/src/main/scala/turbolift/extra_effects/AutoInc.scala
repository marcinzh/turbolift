package turbolift.extra_effects
import turbolift.abstraction.{!!, Effect}


trait AutoIncSig[U] {
  def next: Int !! U
}


trait AutoInc extends Effect[AutoIncSig] {
  def next: Int !! this.type = impureFO(_.next)

  def handler: ThisIHandler[(Int, *)] = handler(0)
  def handler(initial: Int): ThisIHandler[(Int, *)] = AutoIncHandler.apply(this, initial)
}
