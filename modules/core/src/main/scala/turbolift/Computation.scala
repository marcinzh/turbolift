package turbolift
import turbolift.typeclass.MonadPar
import turbolift.internals.effect.{EffectId, AnyChoice}
import turbolift.internals.extensions.ComputationExtensions
import turbolift.std_effects.Choice
import HandlerCases.Primitive
import ComputationCases._


sealed trait Computation[+A, -U]:
  final def map[B](f: A => B): B !! U = new FlatMap(this, f andThen (new Pure(_)))
  final def flatMap[B, V <: U](f: A => B !! V): B !! V = new FlatMap(this, f)
  final def flatten[B, V <: U](implicit ev: A <:< (B !! V)): B !! V = flatMap(ev)
  final def zipPar[B, V <: U](that: B !! V): (A, B) !! V = new ZipPar(this, that)
  final def flatTap[B, V <: U](f: A => B !! V): A !! V = flatMap(a => f(a).as(a))
  final def as[B](value: B): B !! U = map(_ => value)
  final def void: Unit !! U = as(())

  final def >>=[B, V <: U](f: A => B !! V): B !! V = flatMap(f)

  final def *![B, V <: U](that: B !! V): (A, B) !! V = zipPar(that)
  final def **![B, V <: U](that: => B !! V): (A, B) !! V = flatMap(a => that.map((a, _)))

  final def &![B, V <: U](that: B !! V): B !! V = zipPar(that).map(_._2)
  final def &&![B, V <: U](that: => B !! V): B !! V = flatMap(_ => that)

  final def &<![B, V <: U](that: B !! V): A !! V = zipPar(that).map(_._1)
  final def &&<![B, V <: U](that: => B !! V): A !! V = flatMap(a => that.map(_ => a))

  final def |![B >: A, V <: U & Choice](that: B !! V): B !! V = ??? //@#@
  final def ||![B >: A, V <: U & Choice](that: => B !! V): B !! V = AnyChoice.plus(this, that)

  final def withFilter[V <: U & Choice](f: A => Boolean): A !! V = flatMap(a => if f(a) then !!.pure(a) else !!.fail)

  final def upCast[V <: U] = this: A !! V


object Computation extends ComputationExtensions with ComputationInstances:
  val unit = Pure(())
  def pure(): Unit !! Any = unit
  def pure[A](a: A): A !! Any = Pure(a)
  def defer[A, U](ua: => A !! U): A !! U = unit.flatMap(_ => ua)
  def impure[A](a: => A): A !! Any = unit.flatMap(_ => Pure(a))
  def fail: Nothing !! Choice = AnyChoice.empty

  def when[U](cond: Boolean)(body: => Unit !! U): Unit !! U = if cond then body else unit

  def unless[U](cond: Boolean)(body: => Unit !! U): Unit !! U = when(!cond)(body)

  def repeat[U](n: Int)(body: => Unit !! U): Unit !! U =
    def loop(n: Int): Unit !! U =
      if n > 0
      then body &&! loop(n - 1)
      else unit
    loop(n)

  def repeatWhile[U, V <: U](cond: Boolean !! U)(body: => Unit !! V): Unit !! V =
    def loop: Unit !! V = cond.flatMap(if _ then body &&! loop else unit)
    loop

  def repeatUntil[U, V <: U](cond: Boolean !! U)(body: => Unit !! V): Unit !! V =
    def loop: Unit !! V = cond.flatMap(if _ then unit else body &&! loop)
    loop

  def replicate[A, U](n: Int)(body: => A !! U): Vector[A] !! U =
    def loop(n: Int, acc: Vector[A]): Vector[A] !! U =
      if n > 0
      then body.flatMap(a => loop(n - 1, acc :+ a))
      else pure(acc)
    loop(n, Vector())

  def generate[A, B, U](init: A, cond: A => Boolean, inc: A => A)(body: A => B !! U): Vector[B] !! U =
    def loop(a: A, acc: Vector[B]): Vector[B] !! U =
      if cond(a)
      then body(a).flatMap(b => loop(inc(a), acc :+ b))
      else pure(acc)
    loop(init, Vector())

  def iterate[A, U](init: A, cond: A => Boolean, inc: A => A)(body: A => Unit !! U): A !! U =
    def loop(a: A): A !! U =
      if cond(a)
      then body(a) &&! loop(inc(a))
      else pure(a)
    loop(init)

  def iterateVoid[A, U](init: A, cond: A => Boolean, inc: A => A)(body: A => Unit !! U): Unit !! U =
    def loop(a: A): Unit !! U =
      if cond(a)
      then body(a) &&! loop(inc(a))
      else unit
    loop(init)

  def iterateWhile[A, U](init: A, cond: A => Boolean)(body: A => A !! U): A !! U =
    def loop(a: A): A !! U =
      if cond(a)
      then body(a) >>= loop
      else pure(a)
    loop(init)

  def iterateUntil[A, U](init: A, cond: A => Boolean)(body: A => A !! U): A !! U =
    iterateWhile(init, a => !cond(a))(body)


private[turbolift] object ComputationCases:
  final case class Pure[A](value: A) extends Computation[A, Any]
  final case class Lift[A, M[_], U](value: M[A]) extends Computation[A, U]
  final case class FlatMap[A, B, U](that: A !! U, k: A => B !! U) extends Computation[B, U]
  final case class ZipPar[A, B, U](lhs: A !! U, rhs: B !! U) extends Computation[(A, B), U]
  final case class Perform[A, U, Z <: Signature](effectId: EffectId, op: Z => Any) extends Computation[A, U]
  final case class Delimit[A, U, F[+_], L, N](body: A !! (U & L), handler: Primitive[F, L, N]) extends Computation[F[A], U & N]


trait ComputationInstances:
  given [U]: MonadPar[Computation[_, U]] with
    override def pure[A](a: A): A !! U = Pure(a)
    override def flatMap[A, B](ua: A !! U)(f: A => B !! U): B !! U = ua.flatMap(f)
    override def zipPar[A, B](ua: A !! U, ub: B !! U): (A, B) !! U = ua *! ub


type !![+A, -U] = Computation[A, U]
def !! = Computation
