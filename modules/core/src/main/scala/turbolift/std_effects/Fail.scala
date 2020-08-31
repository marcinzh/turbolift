package turbolift.std_effects
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, AlternativeSig}
import turbolift.std_handlers.DefaultFailHandler


trait FailSig[U] extends AlternativeSig[U]


trait Fail extends Effect.Alternative[FailSig] {
  final val fail: Nothing !! this.type = empty
  
  val handler = DefaultFailHandler(this)
}
