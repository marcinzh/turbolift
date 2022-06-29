package turbolift.typeclass
import cats.Id

trait MonadZip[F[_]]:
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]

object MonadZip:
  def apply[F[_]](implicit ev: MonadZip[F]) = ev

  given MonadZip[Id] with
    override def pure[A](a: A): A = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
    override def zip[A, B](a: A, b: B): (A, B) = (a, b)


private trait MonadZipSyntax:
  extension [F[_], A](thiz: F[A])(using F: MonadZip[F])
    def map[B](f: A => B): F[B] = F.map(thiz)(f)
    def flatMap[B](f: A => F[B]): F[B] = F.flatMap(thiz)(f)
    def flatten(implicit ev: A <:< F[A]): F[A] = F.flatMap(thiz)(ev)
    def *![B](that: F[B]): F[(A, B)] = F.zip(thiz, that)
