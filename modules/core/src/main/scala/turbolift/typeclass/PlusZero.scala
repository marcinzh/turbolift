package turbolift.typeclass
import scala.reflect.ClassTag
import scala.collection.immutable.{SortedMap, SortedSet, Queue}


trait PlusZero[T] extends Zero[T] with Plus[T]:
  final def asPlusZero: PlusZero[T] = this


private sealed trait PlusZeroLow:
  given forNumeric[T](using ev: Numeric[T]): PlusZero[T] = AccumZero.forNumeric[T]


object PlusZero extends PlusZeroLow:
  def apply[T](using ev: PlusZero[T]) = ev

  def instance[T](zero: T, plus: (T, T) => T): PlusZero[T] =
    inline def z = zero
    inline def p = plus
    new PlusZero[T]:
      override def zero: T = z
      override def plus(a: T, b: T): T = p(a, b)


  given forUnit: PlusZero[Unit] = AccumZero.forUnit
  given forInt: PlusZero[Int] = AccumZero.forInt
  given forLong: PlusZero[Long] = AccumZero.forLong
  given forString: PlusZero[String] = AccumZero.forString
  given forOption[T](using ev: Plus[T]): PlusZero[Option[T]] = AccumZero.forOption[T]
  given forIArray[T: ClassTag]: PlusZero[IArray[T]] = AccumZero.forIArray[T]
  given forList[T]: PlusZero[List[T]] = AccumZero.forList[T]
  given forVector[T]: PlusZero[Vector[T]] = AccumZero.forVector[T]
  given forQueue[T]: PlusZero[Queue[T]] = AccumZero.forQueue[T]
  given forSet[T]: PlusZero[Set[T]] = AccumZero.forSet[T]
  given forSortedSet[T: Ordering]: PlusZero[SortedSet[T]] = AccumZero.forSortedSet[T]
  given forMap[K, T](using ev: Plus[T]): PlusZero[Map[K, T]] = AccumZero.forMap[K, T, T]
  given forSortedMap[K: Ordering, T](using ev: Plus[T]): PlusZero[SortedMap[K, T]] = AccumZero.forSortedMap[K, T, T]

  def forCollisionlessMap[K, V]: PlusZero[Map[K, V]] = AccumZero.forCollisionlessMap[K, V]
  def forCollisionlessSortedMap[K: Ordering, V]: PlusZero[SortedMap[K, V]] = AccumZero.forCollisionlessSortedMap[K, V]
