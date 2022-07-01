package turbolift
import turbolift.internals.effect.{CanPerform, CanInterpret, HasSignature => Stub}


trait Effect[Z <: Signature] extends CanPerform[Z] with CanInterpret:
  enclosing =>
  final override type ThisEffect = this.type
  final override type ThisSignature = Z
  final override def signatures: Array[Signature] = Array(this)
  final def &![Fx2 <: Stub](fx2: Fx2) = new Effect.Combine2[this.type, Fx2](this, fx2)


object Effect:
  private[turbolift] sealed abstract class Combine(val sigs: Signature*) extends CanInterpret:
    final override val signatures: Array[Signature] = sigs.toArray


  final class Combine2[Fx1 <: Stub, Fx2 <: Stub](val fx1: Fx1, val fx2: Fx2) extends Combine(fx1, fx2):
    override type ThisEffect = fx1.type & fx2.type
    override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature
    def &![Fx3 <: Stub](fx3: Fx3) = new Combine3[Fx1, Fx2, Fx3](fx1, fx2, fx3)


  final class Combine3[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3) extends Combine(fx1, fx2, fx3):
    override type ThisEffect = fx1.type & fx2.type & fx3.type
    override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature
    def &![Fx4 <: Stub](fx4: Fx4) = new Combine4[Fx1, Fx2, Fx3, Fx4](fx1, fx2, fx3, fx4)


  final class Combine4[Fx1 <: Stub, Fx2 <: Stub, Fx3 <: Stub, Fx4 <: Stub](val fx1: Fx1, val fx2: Fx2, val fx3: Fx3, val fx4: Fx4) extends Combine(fx1, fx2, fx3, fx4):
    override type ThisEffect = fx1.type & fx2.type & fx3.type & fx4.type
    override type ThisSignature = fx1.ThisSignature & fx2.ThisSignature & fx3.ThisSignature & fx4.ThisSignature
