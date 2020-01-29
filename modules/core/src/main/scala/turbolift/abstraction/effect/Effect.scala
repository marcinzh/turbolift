package turbolift.abstraction.effect
import turbolift.abstraction.internals.handler.{PrimitiveHandlerStub, SaturatedHandler}


sealed trait AnyEffect[Z[P[_]] <: Signature[P]] extends EffectEncoding[Z] with HasEffectId.Self {
  final override type ThisEffect = this.type

  trait ThisHandler extends PrimitiveHandlerStub {
    final override def effectIdDelegate = AnyEffect.this
    final override type ThisSignature[P[_]] = Z[P]
  }

  trait Nullary[O[_]] extends ThisHandler with SaturatedHandler.Nullary[ThisEffect, O] { def self: Nullary[O] = this }
  trait Unary[S, O[_]] extends ThisHandler with SaturatedHandler.Unary[ThisEffect, S, O] { def self: Unary[S, O] = this }
}


trait Effect[Z[P[_]] <: Signature[P]] extends AnyEffect[Z] {
  trait ThisHandler extends super.ThisHandler {
    final override val isFilterable = false
  }

  trait Nullary[O[_]] extends ThisHandler with super.Nullary[O]
  trait Unary[S, O[_]] extends ThisHandler with super.Unary[S, O]
}


object Effect {
  trait Alternative[Z[P[_]] <: AlternativeSig[P]] extends AnyEffect[Z] with AlternativeEffectEncoding[Z] {
    trait ThisHandler extends super.ThisHandler {
      final override val isFilterable = true
    }

    trait Nullary[O[_]] extends ThisHandler with super.Nullary[O]
    trait Unary[S, O[_]] extends ThisHandler with super.Unary[S, O]
  }
}
