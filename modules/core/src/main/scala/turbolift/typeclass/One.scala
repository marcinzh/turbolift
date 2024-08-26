package turbolift.typeclass
import scala.reflect.ClassTag
import scala.collection.immutable.{SortedMap, SortedSet, Queue}


trait One[T, O]:
  def one(a: O): T

  final def asOne: One[T, O] = this


private sealed trait OneLow:
  given [T]: One[T, T] with
    override def one(a: T): T = a

  given forNumeric[T](using ev: Numeric[T]): One[T, T] = AccumZero.forNumeric[T]


object One extends OneLow:
  def apply[T, O](using ev: One[T, O]) = ev

  def instance[T, O](f: O => T): One[T, O] = new:
    override def one(a: O): T = f(a)


  given forUnit: One[Unit, Unit] = AccumZero.forUnit
  given forInt: One[Int, Int] = AccumZero.forInt
  given forLong: One[Long, Long] = AccumZero.forLong
  given forString: One[String, Char] = AccumZero.forString
  given forOption[T]: One[Option[T], T] = instance(Some(_))
  given forIArray[T: ClassTag]: One[IArray[T], T] = AccumZero.forIArray[T]
  given forList[T]: One[List[T], T] = AccumZero.forList[T]
  given forVector[T]: One[Vector[T], T] = AccumZero.forVector[T]
  given forQueue[T]: One[Queue[T], T] = AccumZero.forQueue[T]
  given forSet[T]: One[Set[T], T] = AccumZero.forSet[T]
  given forSortedSet[T: Ordering]: One[SortedSet[T], T] = AccumZero.forSortedSet[T]
  given forMap[K, T, O](using ev: One[T, O]): One[Map[K, T], (K, O)] = kv => Map((kv._1, ev.one(kv._2)))
  given forSortedMap[K: Ordering, T, O](using ev: One[T, O]): One[SortedMap[K, T], (K, O)] = kv => SortedMap((kv._1, ev.one(kv._2)))

  def forCollisionlessMap[K, V]: One[Map[K, V], (K, V)] = AccumZero.forCollisionlessMap[K, V]
  def forCollisionlessSortedMap[K: Ordering, V]: One[SortedMap[K, V], (K, V)] = AccumZero.forCollisionlessSortedMap[K, V]
