package mwords


trait Semigroup[T] {
  def append(a: T, b: T): T
}

object Semigroup {
  def apply[T](implicit ev: Semigroup[T]) = ev
}

trait SemigroupExports {
  implicit class Semigroup_sentax[T: Semigroup](a: T) {
    def |@|(b: T): T = Semigroup[T].append(a, b)
  }
}


trait Monoid[T] extends Semigroup[T] {
  def empty: T
}

object Monoid {
  def apply[T](implicit ev: Monoid[T]) = ev
}


trait NonEmpty[X, T] {
   def nonEmpty(x: X): T
}

object NonEmpty {
	def apply[X, T](implicit ev: NonEmpty[X, T]) = ev
}
