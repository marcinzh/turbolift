package turbolift.abstraction
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.internals.effect.{EffectId, AnyChoice}
import turbolift.std_effects.Choice
import HandlerCases.Primitive
import ComputationCases._


sealed trait Computation[+A, -U] {
  final def map[B](f: A => B): B !! U = new FlatMap(this, f andThen (new Pure(_)))
  final def flatMap[B, V](f: A => B !! V): B !! U with V = new FlatMap(this, f)
  final def flatten[B, V](implicit ev: A <:< (B !! V)): B !! U with V = flatMap(ev)
  final def zipPar[B, V](that: B !! V): (A, B) !! U with V = new ZipPar(this, that)

  final def *![B, V](that: B !! V): (A, B) !! U with V = zipPar(that)
  final def *<![B, V](that: B !! V): A !! U with V = zipPar(that).map(_._1)
  final def *>![B, V](that: B !! V): B !! U with V = zipPar(that).map(_._2)

  final def **![B, V](that: => B !! V): (A, B) !! U with V = flatMap(a => that.map((a, _)))
  final def **<![B, V](that: => B !! V): A !! U with V = flatMap(a => that.map(_ => a))
  final def **>![B, V](that: => B !! V): B !! U with V = flatMap(_ => that)

  final def &![B, V](that: B !! V): B !! U with V = this *>! that
  final def &&![B, V](that: => B !! V): B !! U with V = this **>! that

  final def |![B >: A, V](that: B !! V): B !! U with V with Choice = ??? //@#@
  final def ||![B >: A, V](that: => B !! V): B !! U with V with Choice = AnyChoice.plus(this, that)

  final def withFilter(f: A => Boolean): A !! U with Choice = flatMap(a => if (f(a)) !!.pure(a) else !!.fail)

  final def void: Unit !! U = map(_ => ())
  final def upCast[V <: U] = this: A !! V
}


object Computation extends ComputationExtensions with ComputationInstances {
  private val pureUnit = Pure(())
  def pure(): Unit !! Any = pureUnit
  def pure[A](a: A): A !! Any = Pure(a)
  def defer[A, U](ua: => A !! U): A !! U = Defer(() => ua)
  def eval[A](a: => A): A !! Any = Defer(() => Pure(a))
  def fail: Nothing !! Choice = AnyChoice.empty
  def when[U](cond: Boolean)(body: => Unit !! U): Unit !! U = if (cond) body else !!.pure()
}


private[abstraction] object ComputationCases {
  final case class Pure[A](value: A) extends Computation[A, Any]
  final case class Done[A, M[_], U](value: M[A]) extends Computation[A, U]
  final case class Defer[A, U](thunk: () => A !! U) extends Computation[A, U]
  final case class FlatMap[A, B, U](that: A !! U, k: A => B !! U) extends Computation[B, U]
  final case class ZipPar[A, B, U](lhs: A !! U, rhs: B !! U) extends Computation[(A, B), U]
  final case class Dispatch[A, U, Z[_]](effectId: EffectId, op: Z[U] => A !! U) extends Computation[A, U]
  final case class Scope[A, U, F[_], L, N](scope: A !! U with L, handler: Primitive[F, L, N]) extends Computation[F[A], U with N]
}


trait ComputationInstances {
  implicit def monad[U]: MonadPar[Computation[*, U]] = new MonadPar[Computation[*, U]] {
    override def pure[A](a: A): A !! U = Pure(a)
    override def flatMap[A, B](ua: A !! U)(f: A => B !! U): B !! U = ua.flatMap(f)
    override def zipPar[A, B](ua: A !! U, ub: B !! U): (A, B) !! U = ua *! ub
    override def defer[A](ua: => A !! U): A !! U = !!.defer(ua)
  }
}


trait ComputationExports {
  type !![+A, -U] = Computation[A, U]
  def !! = Computation
}
