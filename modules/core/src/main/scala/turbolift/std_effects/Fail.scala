package turbolift.std_effects
import turbolift.abstraction.!!
import turbolift.abstraction.effect.Effect
import turbolift.std_handlers.DefaultFailHandler


trait FailSig[U] extends ChoiceSig[U]


trait Fail extends Effect[FailSig] {
  final val fail: Nothing !! this.type = encodeFO(_.empty)
  
  val handler = DefaultFailHandler(this)
}
