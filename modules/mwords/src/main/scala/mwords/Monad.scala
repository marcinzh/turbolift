package mwords


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit ev: Functor[F]) = ev

  def compose[F[_], G[_]](F: Functor[F], G: Functor[G]): Functor[Lambda[X => F[G[X]]]] =
    new Functor[Lambda[X => F[G[X]]]] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(ga => G.map(ga)(f))
    }

  val identity: Functor[Lambda[X => X]] = new Functor[Lambda[X => X]] {
    def map[A, B](a: A)(f: A => B): B = f(a)
  }
}

object FunctorInstances {
  def pair[S]: Functor[(S, +?)] = new Functor[(S, +?)] {
    def map[A, B](sa: (S, A))(f: A => B): (S, B) = (sa._1, f(sa._2))
  }

  val option: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  def either[E]: Functor[Either[E, +?]] = new Functor[Either[E, +?]] {
    def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.map(f)
  }

  val vector: Functor[Vector] = new Functor[Vector] {
    def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }
}

trait FunctorExports {
  implicit class FunctorSyntax[A, F[_]: Functor](thiz: F[A]) {
    def map[B](f: A => B): F[B] = Functor[F].map(thiz)(f)
  }
}


trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
  def zipSeq[A, B](fa: F[A], fb : => F[B]): F[(A, B)] = flatMap(fa)(a => map(fb)((a, _)))
  def zipSeq1st[A, B](fa: F[A], fb : => F[B]): F[A] = flatMap(fa)(a => map(fb)(_ => a))
  def zipSeq2nd[A, B](fa: F[A], fb : => F[B]): F[B] = flatMap(fa)(_ => fb)
}

object Monad {
  def apply[F[_]](implicit ev: Monad[F]) = ev
}

trait MonadExports {
  implicit class MonadSyntax[A, F[_]: Monad](thiz: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(thiz)(f)
    def flatten(implicit ev: A <:< F[A]): F[A] = Monad[F].flatMap(thiz)(ev)
    def **![B](that : => F[B]): F[(A, B)] = Monad[F].zipSeq(thiz, that)
    def **<![B](that : => F[B]): F[A] = Monad[F].zipSeq1st(thiz, that)
    def **>![B](that : => F[B]): F[B] = Monad[F].zipSeq2nd(thiz, that)
  }
}


trait MonadPar[F[_]] extends Monad[F] {
  def zipPar[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def zipPar1st[A, B](fa: F[A], fb: F[B]): F[A] = map(zipPar(fa, fb))(_._1)
  def zipPar2nd[A, B](fa: F[A], fb: F[B]): F[B] = map(zipPar(fa, fb))(_._2)
}

object MonadPar {
  def apply[F[_]](implicit ev: MonadPar[F]) = ev

  val identity: MonadPar[Identity] = new MonadPar[Identity] {
    def pure[A](a: A): A = a
    def flatMap[A, B](a: A)(f: A => B): B = f(a)
    def zipPar[A, B](a: A, b: B): (A, B) = (a, b)
  }
}

trait MonadParExports {
  implicit class MonadParSyntax[F[_]: MonadPar, A](thiz: F[A]) {
    def *![B](that: F[B]): F[(A, B)] = MonadPar[F].zipPar(thiz, that)
    def *<![B](that: F[B]): F[A] = MonadPar[F].zipPar1st(thiz, that)
    def *>![B](that: F[B]): F[B] = MonadPar[F].zipPar2nd(thiz, that)
  }
}
