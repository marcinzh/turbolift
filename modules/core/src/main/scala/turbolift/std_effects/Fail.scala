package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}
import turbolift.std_handlers.DefaultFailHandler


trait FailSig[U] extends ChoiceSig[U]


trait Fail extends Effect[FailSig] {
  final val fail: Nothing !! this.type = embedFO(_.empty)
  
  val handler: ThisHandler[Option] = DefaultFailHandler(this)
}
