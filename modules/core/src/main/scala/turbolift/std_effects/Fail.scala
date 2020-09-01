package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.internals.effect.Signature
import turbolift.std_handlers.DefaultFailHandler


trait FailSig[U] extends ChoiceSig[U]


trait Fail extends Effect[FailSig] {
  final val fail: Nothing !! this.type = encodeFO(_.empty)
  
  val handler = DefaultFailHandler(this)
}
