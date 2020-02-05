package turbolift.abstraction.typeclass
import cats.Semigroup


trait Accum[E, L] {
  def one(e: E): L
  def plus(x: L, y: L): L
  def plus1(x: L, e: E): L // = plus(x, one(e))
}


trait AccumImplicits {
  implicit class AccumSyntax[E, L](x: L)(implicit L: Accum[E, L]) {
    def |+|(y: L): L = L.plus(x, y)
    def |+(e: E): L = L.plus1(x, e)
  }
}


object Accum {
  def apply[E, L](implicit ev: Accum[E, L]) = ev

  import AccumImpl._
  implicit def accumFromSemigroup[L](implicit L: Semigroup[L]): Accum[L, L] = new AccumFromSemigroup[L] { def Semigroup = L }
  implicit def accumForString: Accum[Char, String] = new AccumForString {}
  implicit def accumForVector[E]: Accum[E, Vector[E]] = new AccumForVector[E] {}
  implicit def accumForList[E]: Accum[E, List[E]] = new AccumForList[E] {}
  implicit def accumForSet[E]: Accum[E, Set[E]] = new AccumForSet[E] {}
}


object AccumImpl {
  trait AccumFromSemigroup[L] extends Accum[L, L] {
    def Semigroup: Semigroup[L]
    def one(x: L): L = x
    def plus(x: L, y: L): L = Semigroup.combine(x, y)
    def plus1(x: L, y: L): L = Semigroup.combine(x, y)
  }

  trait AccumForString extends Accum[Char, String] {
    def one(ch: Char) = ch.toString
    def plus(x: String, y: String) = x + y
    def plus1(x: String, c: Char) = x + c.toString
  }

  trait AccumForVector[E] extends Accum[E, Vector[E]] {
    def one(e: E) = Vector(e)
    def plus(xs: Vector[E], ys: Vector[E]) = xs ++ ys
    def plus1(xs: Vector[E], x: E) = xs :+ x
  }

  trait AccumForList[E] extends Accum[E, List[E]] {
    def one(e: E) = List(e)
    def plus(xs: List[E], ys: List[E]) = ys ++ xs //// reversed order
    def plus1(xs: List[E], x: E) = x :: xs
  }

  trait AccumForSet[E] extends Accum[E, Set[E]] {
    def one(e: E) = Set(e)
    def plus(xs: Set[E], ys: Set[E]) = xs | ys
    def plus1(xs: Set[E], x: E) = xs + x
  }
}
