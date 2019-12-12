package turbolift.abstraction.effect
// import mwords._
import turbolift.abstraction.{!!, Return}
import turbolift.abstraction.ComputationCases.Dispatch
import turbolift.abstraction.handlers.{PrimitiveHandler, PrimitiveHandlerBase, SaturatedHandler}


sealed trait AnyEffect[Z <: Signature] extends Signature {
  final type ThisEffect = this.type
  final override type Op[+A] = A !! this.type

  final def pure[A](a: A): A !! this.type = Return(a)
  final def encode[A](f: Z => Z#Op[A]): A !! this.type = new Dispatch[A, this.type, Z](this, f)


  trait ThisHandlerBase extends PrimitiveHandlerBase {
    final override def effectId: AnyRef = AnyEffect.this
    final override type ThisSignature = Z
  }

  trait Nullary[O[+_]] extends SaturatedHandler.Nullary[ThisEffect, O] with ThisHandlerBase
  trait Unary[S, O[+_]] extends SaturatedHandler.Unary[ThisEffect, S, O] with ThisHandlerBase
}


trait Effect[Z <: Signature] extends AnyEffect[Z] {
  trait ThisHandlerBase extends super.ThisHandlerBase {
    final override val isFilterable = false
  }

  trait Nullary[O[+_]] extends super.Nullary[O] with ThisHandlerBase
  trait Unary[S, O[+_]] extends super.Unary[S, O] with ThisHandlerBase
}


sealed trait EffectWithFilter

trait FilterableEffect[Z <: FailSig] extends AnyEffect[Z] with FailSig with EffectWithFilter {
  final val fail = encode(_.fail)

  trait ThisHandlerBase extends super.ThisHandlerBase {
    final override val isFilterable = true
  }

  trait Nullary[O[+_]] extends super.Nullary[O] with ThisHandlerBase
  trait Unary[S, O[+_]] extends super.Unary[S, O] with ThisHandlerBase
}
