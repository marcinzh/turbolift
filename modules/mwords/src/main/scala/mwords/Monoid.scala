package mwords


trait Semigroup[T] {
  def append(a: T, b: T): T
}

object Semigroup {
  def apply[T](implicit ev: Semigroup[T]) = ev

  implicit def forInt: Semigroup[Int] = Monoid[Int]
  implicit def forString: Semigroup[String] = Monoid[String]
  implicit def forVector[T]: Semigroup[Vector[T]] = Monoid[Vector[T]]
  implicit def forList[T]: Semigroup[List[T]] = Monoid[List[T]]
}

trait SemigroupExports {
  implicit class SemigroupSyntax[T: Semigroup](a: T) {
    def |@|(b: T): T = Semigroup[T].append(a, b)
  }
}


trait Monoid[T] extends Semigroup[T] {
  def empty: T
}

object Monoid {
  def apply[T](implicit ev: Monoid[T]) = ev

  implicit val forInt: Monoid[Int] = new Monoid[Int] {
    def empty = 0
    def append(a: Int, b: Int) = a + b
  }

  implicit val forString: Monoid[String] = new Monoid[String] {
    def empty = ""
    def append(a: String, b: String) = a ++ b
  }

  implicit def forList[T]: Monoid[List[T]] = new Monoid[List[T]] {
    def empty = List()
    def append(a: List[T], b: List[T]) = a ++ b
  }

  implicit def forVector[T]: Monoid[Vector[T]] = new Monoid[Vector[T]] {
    def empty = Vector()
    def append(a: Vector[T], b: Vector[T]) = a ++ b
  }
}
