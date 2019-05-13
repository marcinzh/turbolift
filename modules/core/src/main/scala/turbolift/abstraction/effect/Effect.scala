package turbolift.abstraction.effect
// import mwords._
import turbolift.abstraction.{!!, Return}
import turbolift.abstraction.ComputationTags.Dispatch
import turbolift.abstraction.handlers.{ImpureHandler, PartialHandler}


sealed trait AnyEffect[Z <: Signature] extends Signature {
  final type ThisEffect = this.type
  final override type Op[+A] = A !! this.type

  final def pure[A](a: A): A !! this.type = Return(a)
  final def encode[A](f: Z => Z#Op[A]): A !! this.type = new Dispatch[A, this.type, Z](this, f)

  trait ThisHandler[T[_[+_], +_]] extends ImpureHandler[T] {
    final type ThisT[M[+_], +A] = T[M, A]
    final override def effectId: AnyRef = AnyEffect.this
  }

  trait Nullary[F[+_]] extends ThisHandler[Lambda[(`M[+_]`, +[A]) => M[F[A]]]] with PartialHandler.Gimmick {
    final override type Effects = AnyEffect.this.type
    final override type Result[+A] = F[A]
    final override type Trans[M[+_], +A] = ThisT[M, A]
    final override val impure = this
    final override def gimmick[M[+_], A](tma: M[F[A]]): M[F[A]] = tma
  }

  trait Unary[S, F[+_]] extends ThisHandler[Lambda[(`M[+_]`, +[A]) => S => M[F[A]]]] {
    def apply(initial: S): ThisGimmick = new ThisGimmick(initial)
    class ThisGimmick(initial: S) extends PartialHandler.Gimmick {
      final override type Effects = AnyEffect.this.type
      final override type Result[+A] = F[A]
      final override type Trans[M[+_], +A] = ThisT[M, A]
      final override val impure = Unary.this
      final override def gimmick[M[+_], A](tma: S => M[F[A]]): M[F[A]] = tma(initial)
    }
  }
}


trait Effect[Z <: Signature] extends AnyEffect[Z] {
  trait ThisHandler[T[_[+_], +_]] extends super.ThisHandler[T] {
    // final override val empty_?[M[+_] : MonadPar] : Option[T[M, Nothing]] = None
    final override val isFilterable = false
  }

  trait Nullary[F[+_]] extends super.Nullary[F] with ThisHandler[Lambda[(`M[+_]`, +[A]) => M[F[A]]]]
  trait Unary[S, F[+_]] extends super.Unary[S, F] with ThisHandler[Lambda[(`M[+_]`, +[A]) => S => M[F[A]]]]
}


sealed trait EffectWithFilter

trait FilterableEffect[Z <: FailSig] extends AnyEffect[Z] with FailSig with EffectWithFilter {
  final val fail = encode(_.fail)

  trait ThisHandler[T[_[+_], +_]] extends super.ThisHandler[T] {
    trait Decode[M[+_]] extends super.Decode[M] with FailSig
    // final override def empty_?[M[+_] : MonadPar] : Option[T[M, Nothing]] = Some(???)
    final override val isFilterable = true
  }

  trait Nullary[F[+_]] extends super.Nullary[F] with ThisHandler[Lambda[(`M[+_]`, +[A]) => M[F[A]]]]
  trait Unary[S, F[+_]] extends super.Unary[S, F] with ThisHandler[Lambda[(`M[+_]`, +[A]) => S => M[F[A]]]]
}
