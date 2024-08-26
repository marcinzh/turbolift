package turbolift.typeclass
import scala.reflect.ClassTag
import scala.collection.immutable.{SortedMap, SortedSet, Queue}
import scala.math.Numeric


trait Plus[T]:
  def plus(a: T, b: T): T

  final def asPlus: Plus[T] = this


private sealed trait PlusLow:
  given forNumeric[T](using ev: Numeric[T]): Plus[T] = AccumZero.forNumeric[T]


object Plus extends PlusLow:
  def apply[T](using ev: Plus[T]) = ev

  def instance[T](zero: T, plus: (T, T) => T): Plus[T] =
    inline def p = plus
    new Plus[T]:
      override def plus(a: T, b: T): T = p(a, b)


  given forUnit: Plus[Unit] = AccumZero.forUnit
  given forInt: Plus[Int] = AccumZero.forInt
  given forLong: Plus[Long] = AccumZero.forLong
  given forString: Plus[String] = AccumZero.forString
  given forOption[T](using ev: Plus[T]): Plus[Option[T]] = AccumZero.forOption[T]
  given forIArray[T: ClassTag]: Plus[IArray[T]] = AccumZero.forIArray[T]
  given forList[T]: Plus[List[T]] = AccumZero.forList[T]
  given forVector[T]: Plus[Vector[T]] = AccumZero.forVector[T]
  given forQueue[T]: Plus[Queue[T]] = AccumZero.forQueue[T]
  given forSet[T]: Plus[Set[T]] = AccumZero.forSet[T]
  given forSortedSet[T: Ordering]: Plus[SortedSet[T]] = AccumZero.forSortedSet[T]
  given forMap[K, T](using ev: Plus[T]): Plus[Map[K, T]] = AccumZero.forMap[K, T, T]
  given forSortedMap[K: Ordering, T](using ev: Plus[T]): Plus[SortedMap[K, T]] = AccumZero.forSortedMap[K, T, T]

  def forCollisionlessMap[K, V]: Plus[Map[K, V]] = AccumZero.forCollisionlessMap[K, V]
  def forCollisionlessSortedMap[K: Ordering, V]: Plus[SortedMap[K, V]] = AccumZero.forCollisionlessSortedMap[K, V]
