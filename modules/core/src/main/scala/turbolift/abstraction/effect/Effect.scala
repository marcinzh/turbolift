package turbolift.abstraction.effect
import mwords._
import turbolift.abstraction.{!!, Return}
import turbolift.abstraction.ComputationCases.{DispatchFO, DispatchHO}
import turbolift.abstraction.handlers.{PrimitiveHandler, PrimitiveHandlerBase, SaturatedHandler}


sealed trait AnyEffect[Z[P[_]] <: Signature[P]] {
  final type ThisEffect = this.type

  final def pure[A](a: A): A !! this.type = Return(a)

  final def encodeFO[A](f: Z[Phantom] => Phantom[A]): A !! this.type = new DispatchFO(this, f)

  final def encodeHO[U] = new EncodeHO[U with this.type]
  class EncodeHO[U] {
    type Run = (? !! U) ~> Phantom 
    def apply[A](ff: Run => Z[Phantom] => Phantom[A]): A !! U = new DispatchHO(AnyEffect.this, ff) //// DANGER, `this` is shadowed
  }

  type Phantom[A] //// Solves the "Can't existentially abstract over parameterized type" problem

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


sealed trait EffectWithFilter

trait FilterableEffect[Z[P[_]] <: FailSig[P]] extends AnyEffect[Z] with EffectWithFilter {
  final val fail: Nothing !! this.type = encodeFO(_.fail)

  trait ThisHandlerBase extends super.ThisHandlerBase {
    final override val isFilterable = true
  }

  trait Nullary[O[_]] extends ThisHandlerBase with super.Nullary[O]
  trait Unary[S, O[_]] extends ThisHandlerBase with super.Unary[S, O]
}
