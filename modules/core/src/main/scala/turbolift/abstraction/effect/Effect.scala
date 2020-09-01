package turbolift.abstraction.effect
import cats.Functor
import turbolift.abstraction.internals.handler.{PrimitiveHandlerStub, SaturatedHandler}


trait Effect[Z[U] <: Signature[U]] extends EffectEncoding[Z] with HasEffectId.Self {
  final override type ThisEffect = this.type

  trait ThisHandler extends PrimitiveHandlerStub {
    final override def effectIdDelegate = Effect.this
    final override type ThisSignature[U] = Z[U]
  }

  abstract class Nullary[O[_]: Functor] extends SaturatedHandler.Nullary[ThisEffect, O] with ThisHandler {
    final def self: Nullary[O] = this
    final override val theFunctor = Functor[O]
  }

  abstract class Unary[S, O[_]: Functor] extends SaturatedHandler.Unary[ThisEffect, S, O] with ThisHandler {
    final def self: Unary[S, O] = this
    final override val theFunctor = Functor[O]
  }
}
