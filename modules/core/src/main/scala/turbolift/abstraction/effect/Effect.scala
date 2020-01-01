package turbolift.abstraction.effect
import turbolift.abstraction.handlers.{PrimitiveHandler, PrimitiveHandlerBase, SaturatedHandler}


sealed trait AnyEffect[Z[P[_]] <: Signature[P]] extends EffectEncoding[Z] {
  final override type ThisEffect = this.type
  final override def effectId: AnyRef = this

  trait ThisHandlerBase extends PrimitiveHandlerBase {
    final override val effectId: AnyRef = AnyEffect.this
    final override type ThisSignature[P[_]] = Z[P]
  }

  trait Nullary[O[_]] extends ThisHandlerBase with SaturatedHandler.Nullary[ThisEffect, O] { def self: Nullary[O] = this }
  trait Unary[S, O[_]] extends ThisHandlerBase with SaturatedHandler.Unary[ThisEffect, S, O] { def self: Unary[S, O] = this }
}


trait Effect[Z[P[_]] <: Signature[P]] extends AnyEffect[Z] {
  trait ThisHandlerBase extends super.ThisHandlerBase {
    final override val isFilterable = false
  }

  trait Nullary[O[_]] extends ThisHandlerBase with super.Nullary[O]
  trait Unary[S, O[_]] extends ThisHandlerBase with super.Unary[S, O]
}


trait FilterableEffect[Z[P[_]] <: FailSig[P]] extends AnyEffect[Z] with FailEffectEncoding[Z] {
  trait ThisHandlerBase extends super.ThisHandlerBase {
    final override val isFilterable = true
  }

  trait Nullary[O[_]] extends ThisHandlerBase with super.Nullary[O]
  trait Unary[S, O[_]] extends ThisHandlerBase with super.Unary[S, O]
}
