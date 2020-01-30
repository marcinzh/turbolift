package turbolift.abstraction.effect
import mwords.Functor
import turbolift.abstraction.internals.handler.{PrimitiveHandlerStub, SaturatedHandler}


sealed trait AnyEffect[Z[P[_]] <: Signature[P]] extends EffectEncoding[Z] with HasEffectId.Self {
  final override type ThisEffect = this.type

  trait ThisHandler extends PrimitiveHandlerStub {
    final override def effectIdDelegate = AnyEffect.this
    final override type ThisSignature[P[_]] = Z[P]
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


trait Effect[Z[P[_]] <: Signature[P]] extends AnyEffect[Z] {
  trait ThisHandler extends super.ThisHandler {
    final override val isFilterable = false
  }

  abstract class Nullary[O[_]: Functor] extends super.Nullary[O] with ThisHandler
  abstract class Unary[S, O[_]: Functor] extends super.Unary[S, O] with ThisHandler
}


object Effect {
  trait Alternative[Z[P[_]] <: AlternativeSig[P]] extends AnyEffect[Z] with AlternativeEffectEncoding[Z] {
    trait ThisHandler extends super.ThisHandler {
      final override val isFilterable = true
    }

    abstract class Nullary[O[_]: Functor] extends super.Nullary[O] with ThisHandler
    abstract class Unary[S, O[_]: Functor] extends super.Unary[S, O] with ThisHandler
  }
}
