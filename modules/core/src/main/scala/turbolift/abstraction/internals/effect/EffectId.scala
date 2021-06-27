package turbolift.abstraction.internals.effect


sealed trait EffectId extends AnyRef


sealed trait HasEffectId:
  val effectId: EffectId
  // def effectHash: Int


object HasEffectId:
  trait Unsealed extends HasEffectId
  
  trait Self extends HasEffectId with EffectId:
    final override val effectId: EffectId = this
    // final override val effectHash: Int = this.##

  trait Nul extends HasEffectId:
    final override val effectId: EffectId = null
    // final override val effectHash: Int = -1

  // trait Delegate extends HasEffectId:
  //   final override val effectId: EffectId = effectIdDelegate.effectId
  //   // final override val effectHash: Int = effectIdDelegate.effectHash
  //   def effectIdDelegate: HasEffectId
