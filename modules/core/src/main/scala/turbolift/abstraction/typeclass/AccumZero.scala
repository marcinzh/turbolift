package turbolift.abstraction.typeclass
import cats.{Semigroup, Monoid}


trait AccumZero[E, L] extends Accum[E, L] {
  def zero: L
}


object AccumZero {
  def apply[E, L](implicit ev: AccumZero[E, L]) = ev

  import AccumZeroImpl._
  implicit def accumFromMonoid[L](implicit L: Monoid[L]): AccumZero[L, L] = new AccumZeroFromMonoid[L] { def Monoid = L }
  implicit def accumForString: AccumZero[Char, String] = new AccumZeroForString {}
  implicit def accumForVector[E]: AccumZero[E, Vector[E]] = new AccumZeroForVector[E] {}
  implicit def accumForList[E]: AccumZero[E, List[E]] = new AccumZeroForList[E] {}
  implicit def accumForSet[E]: AccumZero[E, Set[E]] = new AccumZeroForSet[E] {}
}


object AccumZeroImpl {
  import AccumImpl._

  trait AccumZeroFromMonoid[L] extends AccumZero[L, L] with AccumFromSemigroup[L] {
    def Monoid: Monoid[L]
    override def Semigroup: Semigroup[L] = Monoid
    def zero: L = Monoid.empty
  }

  trait AccumZeroForString extends AccumZero[Char, String] with AccumForString {
    def zero = ""
  }

  trait AccumZeroForVector[E] extends AccumZero[E, Vector[E]] with AccumForVector[E] {
    def zero = Vector()
  }

  trait AccumZeroForList[E] extends AccumZero[E, List[E]] with AccumForList[E] {
    def zero = List()
  }

  trait AccumZeroForSet[E] extends AccumZero[E, Set[E]] with AccumForSet[E] {
    def zero = Set()
  }
}
