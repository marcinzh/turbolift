package turbolift.typeclass
import scala.collection.IterableFactory
import cats.{Monoid, MonoidK, Alternative}


trait AccumZero[W, W1] extends Accum[W, W1]:
  def zero: W


object AccumZero extends AccumZeroInstances2:
  def apply[W, W1](using ev: AccumZero[W, W1]) = ev

  def collisionlessMap[K, V]: AccumZero[Map[K, V], (K, V)] = makeCollisionlessMap[K, V]


trait AccumZeroInstances1:
  given [W](using W: Monoid[W]): AccumZero[W, W] with
    override def zero: W = W.empty
    override def one(a: W): W = a
    override def plus(a: W, b: W): W = W.combine(a, b)
    override def plus1(a: W, b: W): W = W.combine(a, b)

  given [W, F[_]](using W: Alternative[F]): AccumZero[F[W], W] with
    override def zero: F[W] = W.empty
    override def one(a: W): F[W] = W.pure(a)
    override def plus(a: F[W], b: F[W]): F[W] = W.combineK(a, b)
    override def plus1(a: F[W], b: W): F[W] = W.combineK(a, one(b))


trait AccumZeroInstances2 extends AccumZeroInstances1:
  given forVector[W]: AccumZero[Vector[W], W] = make[Vector, W](Vector, Vector(_), _ :+ _)
  given forList[W]: AccumZero[List[W], W] = make[List, W](List, List(_), _ :+ _)
  given forSet[W]: AccumZero[Set[W], W] = make[Set, W](Set, Set(_), _ + _)

  given forArray[W: reflect.ClassTag]: AccumZero[Array[W], W] with
    override def zero: Array[W] = Array.empty[W]
    override def one(a: W): Array[W] = Array[W](a)
    override def plus(a: Array[W], b: Array[W]): Array[W] = a ++ b
    override def plus1(a: Array[W], b: W): Array[W] = a :+ b

  given forMap[K, V, V1](using V: Accum[V, V1]): AccumZero[Map[K, V], (K, V1)] with
    override def zero: Map[K, V] = Map()
    override def one(kv: (K, V1)): Map[K, V] = Map(kv._1 -> V.one(kv._2))

    override def plus(m1: Map[K, V], m2: Map[K, V]): Map[K, V] =
      m2.foldLeft(m1) {
        case (m, (k, v)) => m.updatedWith(k) {
          case Some(v0) => Some(V.plus(v0, v))
          case None => Some(v)
        }
      }

    override def plus1(m: Map[K, V], kv: (K, V1)): Map[K, V] =
      val (k, v) = kv
      m.updatedWith(k) {
        case Some(v0) => Some(V.plus1(v0, v))
        case None => Some(V.one(v))
      }


private def make[F[X] <: Iterable[X], W](
  factory: IterableFactory[F],
  singleton: W => F[W],
  addOne: (F[W], W) => F[W],
): AccumZero[F[W], W] = 
  new AccumZero[F[W], W]:
    override def zero: F[W] = factory.empty
    override def one(a: W): F[W] = singleton(a)
    override def plus(a: F[W], b: F[W]): F[W] = factory.concat(a, b)
    override def plus1(a: F[W], b: W): F[W] = addOne(a, b)

private def makeCollisionlessMap[K, V]: AccumZero[Map[K, V], (K, V)] =
  new AccumZero[Map[K, V], (K, V)]:
    override def zero: Map[K, V] = Map()
    override def one(kv: (K, V)): Map[K, V] = Map(kv)

    override def plus(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = m2.foldLeft(m1)(plus1)

    override def plus1(m: Map[K, V], kv: (K, V)): Map[K, V] =
      val (k, v) = kv
      m.updatedWith(k) {
        case None => Some(v)
        case _ => sys.error(s"Duplicate key: ${k}")
      }
