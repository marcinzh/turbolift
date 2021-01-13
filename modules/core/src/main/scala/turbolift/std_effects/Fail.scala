package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}


trait FailSig[U] extends ChoiceSig[U]


trait Fail extends Effect[FailSig] {
  final val fail: Nothing !! this.type = embedFO(_.empty)
  
  val handler: ThisIHandler[Option] = FailHandler(this)
}
