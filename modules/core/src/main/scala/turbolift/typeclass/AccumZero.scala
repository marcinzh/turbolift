package turbolift.typeclass
import scala.reflect.ClassTag
import scala.collection.immutable.{SortedMap, SortedSet, Queue}


trait AccumZero[T, O] extends PlusZero[T] with Accum[T, O]


private sealed trait AccumZeroLow:
  given [T](using ev: PlusZero[T]): AccumZero[T, T] with
    override def zero: T = ev.zero
    override def one(a: T): T = a
    override def plus(a: T, b: T): T = ev.plus(a, b)
    override def plus1(a: T, b: T): T = ev.plus(a, b)


  given forNumeric[T](using ev: Numeric[T]): AccumZero[T, T] = new AccumZero.OneIdentity[T]:
    override def zero: T = ev.zero
    override def plus(a: T, b: T): T = ev.plus(a, b)


object AccumZero extends AccumZeroLow:
  def apply[T, O](using ev: AccumZero[T, O]) = ev

  def instance[T, O](zero: T, one: O => T, plus1: (T, O) => T, plus: (T, T) => T): AccumZero[T, O] =
    inline def z = zero
    inline def o = one
    inline def p1 = plus1
    inline def p = plus
    new AccumZero[T, O]:
      override def zero: T = z
      override def one(a: O): T = o(a)
      override def plus1(a: T, b: O): T = p1(a, b)
      override def plus(a: T, b: T): T = p(a, b)

  def instance[T](zero: T, plus: (T, T) => T): AccumZero[T, T] = instance(zero, x => x, plus, plus)

  def instanceEq[T, O](using T =:= O)(zero: T, plus: (T, O) => T): AccumZero[T, O] =
    instance(zero, plus.asInstanceOf[(T, T) => T]).asInstanceOf[AccumZero[T, O]]


  trait OneIdentity[T] extends AccumZero[T, T]:
    final override def one(a: T): T = a
    final override def plus1(a: T, b: T): T = plus(a, b)


  given forUnit: AccumZero[Unit, Unit] = new OneIdentity[Unit]:
    override def zero: Unit = ()
    override def plus(a: Unit, b: Unit): Unit = ()


  given forInt: AccumZero[Int, Int] = new OneIdentity[Int]:
    override def zero: Int = 0
    override def plus(a: Int, b: Int): Int = a + b


  given forLong: AccumZero[Long, Long] = new OneIdentity[Long]:
    override def zero: Long = 0L
    override def plus(a: Long, b: Long): Long = a + b


  given forString: AccumZero[String, Char] = new:
    override def zero: String = ""
    override def one(a: Char): String = a.toString
    override def plus1(a: String, b: Char): String = a :+ b
    override def plus(a: String, b: String): String = a ++ b


  given forOption[T](using ev: Plus[T]): AccumZero[Option[T], T] = new:
    override def zero: Option[T] = None
    override def one(a: T): Option[T] = Some(a)

    override def plus1(aa: Option[T], b: T): Option[T] =
      aa match
        case None => Some(b)
        case Some(a) => Some(ev.plus(a, b))

    override def plus(aa: Option[T], bb: Option[T]): Option[T] =
      aa match
        case None => bb
        case Some(a) => bb match
          case None => aa
          case Some(b) => Some(ev.plus(a, b))


  given forIArray[T: ClassTag]: AccumZero[IArray[T], T] = new:
    override def zero: IArray[T] = IArray.empty[T]
    override def one(a: T): IArray[T] = IArray(a)
    override def plus1(aa: IArray[T], b: T): IArray[T] = aa :+ b
    override def plus(aa: IArray[T], bb: IArray[T]): IArray[T] = aa ++ bb


  given forList[T]: AccumZero[List[T], T] = new:
    override def zero: List[T] = Nil
    override def one(a: T): List[T] = a :: Nil
    override def plus1(aa: List[T], b: T): List[T] = aa :+ b
    override def plus(aa: List[T], bb: List[T]): List[T] = aa ++ bb


  given forVector[T]: AccumZero[Vector[T], T] = new:
    override def zero: Vector[T] = Vector.empty[T]
    override def one(a: T): Vector[T] = Vector(a)
    override def plus1(aa: Vector[T], b: T): Vector[T] = aa :+ b
    override def plus(aa: Vector[T], bb: Vector[T]): Vector[T] = aa ++ bb


  given forQueue[T]: AccumZero[Queue[T], T] = new:
    override def zero: Queue[T] = Queue.empty[T]
    override def one(a: T): Queue[T] = Queue(a)
    override def plus1(aa: Queue[T], b: T): Queue[T] = aa :+ b
    override def plus(aa: Queue[T], bb: Queue[T]): Queue[T] = aa ++ bb


  given forSet[T]: AccumZero[Set[T], T] = new:
    override def zero: Set[T] = Set.empty[T]
    override def one(a: T): Set[T] = Set(a)
    override def plus1(aa: Set[T], b: T): Set[T] = aa + b
    override def plus(aa: Set[T], bb: Set[T]): Set[T] = aa | bb


  given forSortedSet[T: Ordering]: AccumZero[SortedSet[T], T] = new:
    override def zero: SortedSet[T] = SortedSet.empty[T]
    override def one(a: T): SortedSet[T] = SortedSet(a)
    override def plus1(aa: SortedSet[T], b: T): SortedSet[T] = aa + b
    override def plus(aa: SortedSet[T], bb: SortedSet[T]): SortedSet[T] = aa | bb


  given forMap[K, T, O](using ev: Accum[T, O]): AccumZero[Map[K, T], (K, O)] = new:
    override def zero: Map[K, T] = Map.empty[K, T]
    override def one(kv: (K, O)): Map[K, T] = Map((kv._1, ev.one(kv._2)))

    override def plus1(aa: Map[K, T], kv: (K, O)): Map[K, T] =
      val (k, v) = kv
      aa.updatedWith(k):
        case Some(v0) => Some(ev.plus1(v0, v))
        case None => Some(ev.one(v))

    override def plus(aa: Map[K, T], bb: Map[K, T]): Map[K, T] =
      bb.foldLeft(aa):
        case (m, (k, v)) => m.updatedWith(k):
          case Some(v0) => Some(ev.plus(v0, v))
          case None => Some(v)


  given forSortedMap[K: Ordering, T, O](using ev: Accum[T, O]): AccumZero[SortedMap[K, T], (K, O)] = new:
    override def zero: SortedMap[K, T] = SortedMap.empty[K, T]
    override def one(kv: (K, O)): SortedMap[K, T] = SortedMap((kv._1, ev.one(kv._2)))

    override def plus1(aa: SortedMap[K, T], kv: (K, O)): SortedMap[K, T] =
      val (k, v) = kv
      aa.updatedWith(k):
        case Some(v0) => Some(ev.plus1(v0, v))
        case None => Some(ev.one(v))

    override def plus(aa: SortedMap[K, T], bb: SortedMap[K, T]): SortedMap[K, T] =
      bb.foldLeft(aa):
        case (m, (k, v)) => m.updatedWith(k):
          case Some(v0) => Some(ev.plus(v0, v))
          case None => Some(v)


  def forCollisionlessMap[K, V]: AccumZero[Map[K, V], (K, V)] = new:
    override def zero: Map[K, V] = Map.empty[K, V]
    override def one(kv: (K, V)): Map[K, V] = Map(kv)

    override def plus1(aa: Map[K, V], kv: (K, V)): Map[K, V] =
      val (k, v) = kv
      aa.updatedWith(k):
        case None => Some(v)
        case _ => throw KeyCollision(k)

    override def plus(aa: Map[K, V], bb: Map[K, V]): Map[K, V] = aa.foldLeft(bb)(plus1)


  def forCollisionlessSortedMap[K: Ordering, V]: AccumZero[SortedMap[K, V], (K, V)] = new:
    override def zero: SortedMap[K, V] = SortedMap.empty[K, V]
    override def one(kv: (K, V)): SortedMap[K, V] = SortedMap(kv)

    override def plus1(aa: SortedMap[K, V], kv: (K, V)): SortedMap[K, V] =
      val (k, v) = kv
      aa.updatedWith(k):
        case None => Some(v)
        case _ => throw KeyCollision(k)

    override def plus(aa: SortedMap[K, V], bb: SortedMap[K, V]): SortedMap[K, V] = aa.foldLeft(bb)(plus1)


  final class KeyCollision(key: Any) extends RuntimeException(s"Map key collision: $key")
