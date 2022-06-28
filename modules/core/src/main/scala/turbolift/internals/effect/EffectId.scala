package turbolift.internals.effect


private[turbolift] sealed trait EffectId extends AnyRef


private[turbolift] sealed trait HasEffectId:
  val effectId: EffectId
  // def effectHash: Int


private[turbolift] object HasEffectId:
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
