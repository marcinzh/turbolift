package turbolift.typeclass
import scala.reflect.ClassTag
import scala.collection.immutable.{SortedMap, SortedSet, Queue}


trait Accum[T, O] extends One[T, O] with Plus[T]:
  def plus1(a: T, b: O): T //// == plus(a, one(b))

  final def asAccum: Accum[T, O] = this


private sealed trait AccumLow:
  given [T](using ev: Plus[T]): Accum[T, T] with
    override def one(a: T): T = a
    override def plus(a: T, b: T): T = ev.plus(a, b)
    override def plus1(a: T, b: T): T = ev.plus(a, b)

  given forNumeric[T](using ev: Numeric[T]): Accum[T, T] = AccumZero.forNumeric[T]


object Accum extends AccumLow:
  def apply[T, O](using ev: Accum[T, O]) = ev

  def instance[T, O](one: O => T, plus1: (T, O) => T, plus: (T, T) => T): Accum[T, O] =
    inline def o = one
    inline def p1 = plus1
    inline def p = plus
    new Accum[T, O]:
      override def one(a: O): T = o(a)
      override def plus1(a: T, b: O): T = p1(a, b)
      override def plus(a: T, b: T): T = p(a, b)

  def instance[T](plus: (T, T) => T): Accum[T, T] = instance(x => x, plus, plus)

  def instanceEq[T, O](using T =:= O)(plus: (T, O) => T): Accum[T, O] =
    instance(plus.asInstanceOf[(T, T) => T]).asInstanceOf[Accum[T, O]]


  given forUnit: Accum[Unit, Unit] = AccumZero.forUnit
  given forInt: Accum[Int, Int] = AccumZero.forInt
  given forLong: Accum[Long, Long] = AccumZero.forLong
  given forString: Accum[String, Char] = AccumZero.forString
  given forOption[T](using ev: Plus[T]): Accum[Option[T], T] = AccumZero.forOption[T]
  given forIArray[T: ClassTag]: Accum[IArray[T], T] = AccumZero.forIArray[T]
  given forList[T]: Accum[List[T], T] = AccumZero.forList[T]
  given forVector[T]: Accum[Vector[T], T] = AccumZero.forVector[T]
  given forQueue[T]: Accum[Queue[T], T] = AccumZero.forQueue[T]
  given forSet[T]: Accum[Set[T], T] = AccumZero.forSet[T]
  given forSortedSet[T: Ordering]: Accum[SortedSet[T], T] = AccumZero.forSortedSet[T]
  given forMap[K, T, O](using ev: Accum[T, O]): Accum[Map[K, T], (K, O)] = AccumZero.forMap[K, T, O]
  given forSortedMap[K: Ordering, T, O](using ev: Accum[T, O]): Accum[SortedMap[K, T], (K, O)] = AccumZero.forSortedMap[K, T, O]

  def forCollisionlessMap[K, V]: Accum[Map[K, V], (K, V)] = AccumZero.forCollisionlessMap[K, V]
  def forCollisionlessSortedMap[K: Ordering, V]: Accum[SortedMap[K, V], (K, V)] = AccumZero.forCollisionlessSortedMap[K, V]

