package turbolift.abstraction
import mwords.{MonadPar, ~>}
import turbolift.abstraction.effect.{EffectId, Signature, FailEffect}
import turbolift.abstraction.internals.handler.SaturatedHandler
import turbolift.abstraction.internals.interpreter.Interpreter
import turbolift.abstraction.internals.aux.{CanRunPure, CanRunImpure, CanHandle}
import ComputationCases._


sealed trait Computation[+A, -U] {
  final def map[B](f: A => B): B !! U = new FlatMap(this, f andThen (new Pure(_)))
  final def flatMap[B, V](f: A => B !! V): B !! U with V = new FlatMap(this, f)
  final def flatten[B, V](implicit ev: A <:< (B !! V)): B !! U with V = flatMap(ev)
  final def zipPar[B, V](that: B !! V): (A, B) !! U with V = new ZipPar(this, that)

  final def *![B, V](that: B !! V): (A, B) !! U with V = zipPar(that)
  final def *<![B, V](that: B !! V): A !! U with V = zipPar(that).map(_._1)
  final def *>![B, V](that: B !! V): B !! U with V = zipPar(that).map(_._2)

  final def **![B, V](that : => B !! V): (A, B) !! U with V = flatMap(a => that.map((a, _)))
  final def **<![B, V](that : => B !! V): A !! U with V = flatMap(a => that.map(_ => a))
  final def **>![B, V](that : => B !! V): B !! U with V = flatMap(_ => that)

  final def &![B, V](that: B !! V): B !! U with V = this *>! that
  final def &&![B, V](that : => B !! V): B !! U with V = this **>! that

  final def |?[B >: A, V](that: B !! V): B !! U with V with FailEffect = FailEffect.orElsePar(this, that)
  final def ||?[B >: A, V](that: => B !! V): B !! U with V with FailEffect = FailEffect.orElseSeq(this, that)

  final def withFilter(f: A => Boolean): A !! U with FailEffect = flatMap(a => if (f(a)) !!.pure(a) else !!.fail)

  final def void: Unit !! U = map(_ => ())
  final def upCast[V <: U] = this: A !! V
  final def forceFilterable = this: A !! U with FailEffect
}


object Computation {
  def pure(): Unit !! Any = Return()
  def pure[A](a: A): A !! Any = Return(a)
  def fail: Nothing !! FailEffect = FailEffect.fail
  def defer[A, U](ua: => A !! U): A !! U = Return().flatMap(_ => ua)
}


object Return {
  private val unit = apply(())
  def apply(): Unit !! Any = unit
  def apply[A](a: A): A !! Any = new Pure(a)
}


private[abstraction] object ComputationCases {
  final case class Pure[A](value: A) extends Computation[A, Any]
  final case class FlatMap[A, B, U](that: A !! U, k: A => B !! U) extends Computation[B, U]
  final case class ZipPar[A, B, U](lhs: A !! U, rhs: B !! U) extends Computation[(A, B), U]
  final case class DispatchFO[A, U, Z[P[_]] <: Signature[P], P[_]](effectId: EffectId, op: Z[P] => P[A]) extends Computation[A, U]
  final case class DispatchHO[A, U, Z[P[_]] <: Signature[P], P[_]](effectId: EffectId, op: ((? !! U) ~> P) => Z[P] => P[A]) extends Computation[A, U]
  final case class PushHandler[A, U, H <: SaturatedHandler](scope: A !! U with H#Effects, h: H) extends Computation[H#Result[A], U]
}


object ComputationInstances {
  implicit def monad[U]: MonadPar[Computation[?, U]] = new MonadPar[Computation[?, U]] {
    def pure[A](a: A): A !! U = Return(a)
    def flatMap[A, B](ma: A !! U)(f: A => B !! U): B !! U = ma.flatMap(f)
    def zipPar[A, B](ma: A !! U, mb: B !! U): (A, B) !! U = ma *! mb
  }
}


trait ComputationExports {
  type !![+A, -U] = Computation[A, U]
  def !! = Computation

  implicit class ComputationExtension[A, U](thiz: A !! U) {
    def run(implicit ev: CanRunPure[U]): A = (Interpreter.pure(ev(thiz))).run
    def runStackUnsafe(implicit ev: CanRunPure[U]): A = (Interpreter.pureStackUnsafe[A](ev(thiz)))
    
    def runWith[H <: Handler](h: H)(implicit ev: CanRunImpure[U, h.Effects]): h.Result[A] =
      h.doHandle[A, Any](ev(thiz)).run

    def runStackUnsafeWith[H <: Handler](h: H)(implicit ev: CanRunImpure[U, h.Effects]): h.Result[A] =
      h.doHandle[A, Any](ev(thiz)).runStackUnsafe

    def handleWith[V] : HandleWithApply[V] = new HandleWithApply[V]
    class HandleWithApply[V] {
      def apply[H <: Handler](h: H)(implicit ev: CanHandle[V, U, h.Effects]): h.Result[A] !! V =
        h.doHandle[A, V](ev(thiz))
    }

    def downCast[V >: U] = thiz.asInstanceOf[Computation[A, V]]
  }
}
