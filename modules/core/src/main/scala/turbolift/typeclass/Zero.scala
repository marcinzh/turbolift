package turbolift.typeclass
import scala.reflect.ClassTag
import scala.collection.immutable.{SortedMap, SortedSet, Queue}
import scala.math.Numeric


trait Zero[T]:
  def zero: T

  final def asZero: Zero[T] = this


private sealed trait ZeroLow:
  given forNumeric[T](using ev: Numeric[T]): Zero[T] = AccumZero.forNumeric[T]


object Zero extends ZeroLow:
  def apply[T](using ev: Zero[T]) = ev

  def instance[T](zero: T): Zero[T] =
    inline def z = zero
    new Zero[T]:
      override def zero: T = z

  given forUnit: Zero[Unit] = AccumZero.forUnit
  given forInt: Zero[Int] = AccumZero.forInt
  given forLong: Zero[Long] = AccumZero.forLong
  given forString: Zero[String] = AccumZero.forString
  given forOption[T]: Zero[Option[T]] = instance(None)
  given forIArray[T: ClassTag]: Zero[IArray[T]] = AccumZero.forIArray[T]
  given forList[T]: Zero[List[T]] = AccumZero.forList[T]
  given forVector[T]: Zero[Vector[T]] = AccumZero.forVector[T]
  given forQueue[T]: Zero[Queue[T]] = AccumZero.forQueue[T]
  given forSet[T]: Zero[Set[T]] = AccumZero.forSet[T]
  given forSortedSet[T: Ordering]: Zero[SortedSet[T]] = AccumZero.forSortedSet[T]
  given forMap[K, T]: Zero[Map[K, T]] = instance(Map())
  given forSortedMap[K: Ordering, T]: Zero[SortedMap[K, T]] = instance(SortedMap())
