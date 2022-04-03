package turbolift.typeclass
import cats.Id

trait MonadPar[F[_]]:
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def zipPar[A, B](fa: F[A], fb: F[B]): F[(A, B)]

object MonadPar:
  def apply[F[_]](implicit ev: MonadPar[F]) = ev

  given MonadPar[Id] with
    override def pure[A](a: A): A = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
    override def zipPar[A, B](a: A, b: B): (A, B) = (a, b)


trait MonadParSyntax:
  extension [F[_], A](thiz: F[A])(using F: MonadPar[F])
    def map[B](f: A => B): F[B] = F.map(thiz)(f)
    def flatMap[B](f: A => F[B]): F[B] = F.flatMap(thiz)(f)
    def flatten(implicit ev: A <:< F[A]): F[A] = F.flatMap(thiz)(ev)
    def *![B](that: F[B]): F[(A, B)] = F.zipPar(thiz, that)
