package mwords


trait Zero[L] {
  def zero: L
}

object Zero {
  def apply[L](implicit ev: Zero[L]) = ev

  import ZeroImpl._
  implicit def zeroForString: Zero[String] = new ZeroForString {}
  implicit def zeroForVector[L]: Zero[Vector[L]] = new ZeroForVector[L] {}
  implicit def zeroForList[L]: Zero[List[L]] = new ZeroForList[L] {}
  implicit def zeroForSet[L]: Zero[Set[L]] = new ZeroForSet[L] {}
}

object ZeroImpl {
  trait ZeroForString extends Zero[String] {
    def zero = ""
  }

  trait ZeroForVector[E] extends Zero[Vector[E]] {
    def zero = Vector()
  }

  trait ZeroForList[E] extends Zero[List[E]] {
    def zero = List()
  }

  trait ZeroForSet[E] extends Zero[Set[E]] {
    def zero = Set()
  }
}


trait One[E, L] {
  def one(e: E): L
}

object One {
  def apply[E, L](implicit ev: One[E, L]) = ev

  import OneImpl._
  implicit def oneForString: One[Char, String] = new OneForString {}
  implicit def oneForVector[E]: One[E, Vector[E]] = new OneForVector[E] {}
  implicit def oneForList[E]: One[E, List[E]] = new OneForList[E] {}
  implicit def oneForSet[E]: One[E, Set[E]] = new OneForSet[E] {}
}

object OneImpl {
  trait OneForString extends One[Char, String] {
    def one(ch: Char) = ch.toString
  }

  trait OneForVector[E] extends One[E, Vector[E]] {
    def one(e: E) = Vector(e)
  }

  trait OneForList[E] extends One[E, List[E]] {
    def one(e: E) = List(e)
  }

  trait OneForSet[E] extends One[E, Set[E]] {
    def one(e: E) = Set(e)
  }
}


trait PlusOne[E, L] extends One[E, L] {
  def plus(x: L, y: L): L
  def plus1(x: L, e: E): L
}

object PlusOne {
  def apply[E, L](implicit ev: PlusOne[E, L]) = ev

  import PlusOneImpl._
  implicit def plusOneFromSemigroup[L](implicit L: Semigroup[L]): PlusOne[L, L] = new PlusOneFromSemigroup[L] { def Semigroup = L }
  implicit def plusOneForString: PlusOne[Char, String] = new PlusOneForString {}
  implicit def plusOneForVector[E]: PlusOne[E, Vector[E]] = new PlusOneForVector[E] {}
  implicit def plusOneForList[E]: PlusOne[E, List[E]] = new PlusOneForList[E] {}
  implicit def plusOneForSet[E]: PlusOne[E, Set[E]] = new PlusOneForSet[E] {}
}


object PlusOneImpl {
  import OneImpl._

  trait PlusOneFromSemigroup[L] extends PlusOne[L, L] {
    implicit def Semigroup: Semigroup[L]
    def one(x: L): L = x
    def plus(x: L, y: L): L = Semigroup.append(x, y)
    def plus1(x: L, y: L): L = Semigroup.append(x, y)
  }

  trait PlusOneForString extends PlusOne[Char, String] with OneForString {
    def plus(x: String, y: String) = x + y
    def plus1(x: String, c: Char) = x + c.toString
  }

  trait PlusOneForVector[E] extends PlusOne[E, Vector[E]] with OneForVector[E] {
    def plus(xs: Vector[E], ys: Vector[E]) = xs ++ ys
    def plus1(xs: Vector[E], x: E) = xs :+ x
  }

  trait PlusOneForList[E] extends PlusOne[E, List[E]] with OneForList[E] {
    def plus(xs: List[E], ys: List[E]) = ys ++ xs //// reverse order
    def plus1(xs: List[E], x: E) = x :: xs
  }

  trait PlusOneForSet[E] extends PlusOne[E, Set[E]] with OneForSet[E] {
    def plus(xs: Set[E], ys: Set[E]) = xs | ys
    def plus1(xs: Set[E], x: E) = xs + x
  }
}


trait PlusOneZero[E, L] extends PlusOne[E, L] with Zero[L]

object PlusOneZero {
  def apply[E, L](implicit ev: PlusOneZero[E, L]) = ev

  import PlusOneZeroImpl._
  implicit def plusOneZeroFromMonoid[L](implicit L: Monoid[L]): PlusOneZero[L, L] = new PlusOneZeroFromMonoid[L] { def Monoid = L }
  implicit def plusOneZeroForString: PlusOneZero[Char, String] = new PlusOneZeroForString {}
  implicit def plusOneZeroForList[E]: PlusOneZero[E, List[E]] = new PlusOneZeroForList[E] {}
  implicit def plusOneZeroForVector[E]: PlusOneZero[E, Vector[E]] = new PlusOneZeroForVector[E] {}
  implicit def plusOneZeroForSet[E]: PlusOneZero[E, Set[E]] = new PlusOneZeroForSet[E] {}
}

object PlusOneZeroImpl {
  import ZeroImpl._
  import PlusOneImpl._

  trait PlusOneZeroFromMonoid[L] extends PlusOneZero[L, L] with PlusOneFromSemigroup[L] {
    def Semigroup: Semigroup[L] = Monoid
    def Monoid: Monoid[L]
    def zero: L = Monoid.empty
  }

  trait PlusOneZeroForString extends PlusOneZero[Char, String] with PlusOneForString with ZeroForString
  trait PlusOneZeroForList[E] extends PlusOneZero[E, List[E]] with PlusOneForList[E] with ZeroForList[E]
  trait PlusOneZeroForVector[E] extends PlusOneZero[E, Vector[E]] with PlusOneForVector[E] with ZeroForVector[E]
  trait PlusOneZeroForSet[E] extends PlusOneZero[E, Set[E]] with PlusOneForSet[E] with ZeroForSet[E]
}

trait PlusOneExports {
  implicit class PlusOneSyntax[E, L](x: L)(implicit L: PlusOne[E, L]) {
    def |+|(y: L): L = L.plus(x, y)
    def |+(e: E): L = L.plus1(x, e)
  }
}
