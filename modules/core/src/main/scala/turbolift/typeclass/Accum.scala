package turbolift.typeclass
import cats.{Semigroup, SemigroupK, Applicative}


trait Accum[W, W1] extends One[W, W1]:
  def plus(x: W, y: W): W
  def plus1(x: W, e: W1): W // = plus(x, one(e))


object Accum extends AccumInstances2:
  def apply[W, W1](using ev: Accum[W, W1]) = ev


private trait AccumInstances1:
  given [W](using W: Semigroup[W]): Accum[W, W] with
    override def one(a: W): W = a
    override def plus(a: W, b: W): W = W.combine(a, b)
    override def plus1(a: W, b: W): W = W.combine(a, b)

  given [W, F[_]](using W: SemigroupK[F], Appl: Applicative[F]): Accum[F[W], W] with
    override def one(a: W): F[W] = Appl.pure(a)
    override def plus(a: F[W], b: F[W]): F[W] = W.combineK(a, b)
    override def plus1(a: F[W], b: W): F[W] = W.combineK(a, one(b))


private trait AccumInstances2 extends AccumInstances1:
  given forVector[W]: Accum[Vector[W], W] = AccumZero.forVector[W]
  given forList[W]: Accum[List[W], W] = AccumZero.forList[W]
  given forSet[W]: Accum[Set[W], W] = AccumZero.forSet[W]
  given forArray[W: reflect.ClassTag]: Accum[Array[W], W] = AccumZero.forArray[W]
  given forMap[K, V, V1](using V: Accum[V, V1]): Accum[Map[K, V], (K, V1)] = AccumZero.forMap[K, V, V1]


private trait AccumSyntax:
  extension [W, W1](thiz: W)(using W: Accum[W, W1])
    def |+|(that: W): W = W.plus(thiz, that)
    def |+(that: W1): W = W.plus1(thiz, that)
