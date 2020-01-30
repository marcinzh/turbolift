package mwords


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor extends FunctorInstances {
  def apply[F[_]](implicit ev: Functor[F]) = ev

  def compose[F[_], G[_]](F: Functor[F], G: Functor[G]): Functor[Lambda[X => F[G[X]]]] =
    new Functor[Lambda[X => F[G[X]]]] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(ga => G.map(ga)(f))
    }
}

trait FunctorExports {
  implicit class FunctorSyntax[A, F[_]: Functor](thiz: F[A]) {
    def map[B](f: A => B): F[B] = Functor[F].map(thiz)(f)
  }
}

trait FunctorInstances {
  implicit val identity: Functor[Lambda[X => X]] = new Functor[Lambda[X => X]] {
    def map[A, B](a: A)(f: A => B): B = f(a)
  }

  implicit def pair[S]: Functor[(S, +?)] = new Functor[(S, +?)] {
    def map[A, B](sa: (S, A))(f: A => B): (S, B) = (sa._1, f(sa._2))
  }

  implicit val option: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def either[E]: Functor[Either[E, +?]] = new Functor[Either[E, +?]] {
    def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.map(f)
  }

  implicit val vector: Functor[Vector] = new Functor[Vector] {
    def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
}

object Applicative {
  def apply[F[_]](implicit ev: Applicative[F]) = ev

  def compose[F[_], G[_]](F: Applicative[F], G: Applicative[G]): Applicative[Lambda[X => F[G[X]]]] =
    new Applicative[Lambda[X => F[G[X]]]] {
      def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(ga => G.map(ga)(f))
      def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] = F.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
    }

  val identity: Applicative[Lambda[X => X]] = new Applicative[Lambda[X => X]] {
    def pure[A](a: A): A = a
    def map[A, B](a: A)(f: A => B): B = f(a)
    def map2[A, B, C](a: A, b: B)(f: (A, B) => C): C = f(a, b)
  }
}

trait ApplicativeExports {
  implicit class ApplicativeSyntax[A, F[_]: Applicative](thiz: F[A]) {
    def map2[B, C](that: F[B])(f: (A, B) => C): F[C] = Applicative[F].map2(thiz, that)(f)
  }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
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
  def defer[A](th: () => F[A]): F[A]
}

object MonadPar {
  def apply[F[_]](implicit ev: MonadPar[F]) = ev

  val identity: MonadPar[Identity] = new MonadPar[Identity] {
    def pure[A](a: A): A = a
    def flatMap[A, B](a: A)(f: A => B): B = f(a)
    def zipPar[A, B](a: A, b: B): (A, B) = (a, b)
    def defer[A](th: () => A): A = th()
  }
}

trait MonadParExports {
  implicit class MonadParSyntax[F[_]: MonadPar, A](thiz: F[A]) {
    def *![B](that: F[B]): F[(A, B)] = MonadPar[F].zipPar(thiz, that)
    def *<![B](that: F[B]): F[A] = MonadPar[F].zipPar1st(thiz, that)
    def *>![B](that: F[B]): F[B] = MonadPar[F].zipPar2nd(thiz, that)
  }
}

/*
trait MonadBasePar[F[_]] extends MonadPar[F] {
  def defer[A](th: () => F[A]): F[A]
}


object MonadBasePar {
  def apply[F[_]](implicit ev: MonadBasePar[F]) = ev

  val identity: MonadBasePar[Identity] = new MonadBasePar[Identity] {
    def pure[A](a: A): A = a
    def flatMap[A, B](a: A)(f: A => B): B = f(a)
    def zipPar[A, B](a: A, b: B): (A, B) = (a, b)
    def defer[A](th: () => A): A = th()
  }
}
*/