package turbolift.effects
import scala.util.{Random => ScalaRandom}
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.data.Splitmix64


/** Signature of [[RandomEffect]]. */
trait RandomSignature extends Signature:
  def nextBoolean: Boolean !! ThisEffect
  def nextInt: Int !! ThisEffect
  def nextInt(n: Int): Int !! ThisEffect
  def nextLong: Long !! ThisEffect
  def nextLong(n: Long): Long !! ThisEffect
  def nextFloat: Float !! ThisEffect
  def nextDouble: Double !! ThisEffect
  def nextGaussian: Double !! ThisEffect
  def between(minInclusive: Int, maxExclusive: Int): Int !! ThisEffect
  def between(minInclusive: Long, maxExclusive: Long): Long !! ThisEffect
  def between(minInclusive: Float, maxExclusive: Float): Float !! ThisEffect
  def between(minInclusive: Double, maxExclusive: Double): Double !! ThisEffect
  def nextBytes(n: Int): Array[Byte] !! ThisEffect
  def setSeed(seed: Long): Unit !! ThisEffect


/** Base trait for custom instances of Random effect.
 *
 * {{{
 * case object MyRandom extends RandomEffect
 * // optional:
 * type MyRandom = MyRandom.type
 * }}}
 *
 * Effectful version of [[scala.util.Random]]
 *
 * @see [[Random]]
 */
trait RandomEffect extends Effect[RandomSignature] with RandomSignature:
  enclosing =>
  final override def nextBoolean: Boolean !! this.type = perform(_.nextBoolean)
  final override def nextInt: Int !! this.type = perform(_.nextInt)
  final override def nextInt(n: Int): Int !! this.type = perform(_.nextInt(n))
  final override def nextLong: Long !! this.type = perform(_.nextLong)
  final override def nextLong(n: Long): Long !! this.type = perform(_.nextLong(n))
  final override def nextFloat: Float !! this.type = perform(_.nextFloat)
  final override def nextDouble: Double !! this.type = perform(_.nextDouble)
  final override def nextGaussian: Double !! this.type = perform(_.nextGaussian)
  final override def between(minInclusive: Int, maxExclusive: Int): Int !! this.type = perform(_.between(minInclusive, maxExclusive))
  final override def between(minInclusive: Long, maxExclusive: Long): Long !! this.type = perform(_.between(minInclusive, maxExclusive))
  final override def between(minInclusive: Float, maxExclusive: Float): Float !! this.type = perform(_.between(minInclusive, maxExclusive))
  final override def between(minInclusive: Double, maxExclusive: Double): Double !! this.type = perform(_.between(minInclusive, maxExclusive))
  final override def nextBytes(n: Int): Array[Byte] !! ThisEffect = perform(_.nextBytes(n))
  final override def setSeed(seed: Long): Unit !! this.type = perform(_.setSeed(seed))

  /** Predefined handlers for this effect. */
  object handlers:
    private def randomSeed: Long !! IO = IO.sync(ScalaRandom.nextLong)

    def local: Handler[Identity, Identity, enclosing.type, IO] = randomSeed.flatMapHandler(local(_))
    def shared: Handler[Identity, Identity, enclosing.type, IO] = randomSeed.flatMapHandler(shared(_))

    /** Deterministic, even under parallelism. */
    def local(seed: Long): Handler[Identity, Identity, enclosing.type, Any] =
      new impl.Stateful[Identity, (_, Splitmix64), Any] with impl.Parallel.ForkJoin with RandomSignature:
        override type Local = Splitmix64
        override def onInitial: Local !! Any = !!.pure(Splitmix64(seed))
        override def onReturn(a: Unknown, s: Splitmix64): (Unknown, Splitmix64) !! Any = !!.pure((a, s))
        override def onRestart(aa: (Unknown, Splitmix64)) = enclosing.setSeed(aa._2.value) &&! !!.pure(aa._1)
        override def onUnknown(aa: (Unknown, Splitmix64)): Option[Unknown] = Some(aa._1)
        override def onFork(s: Splitmix64): (Splitmix64, Splitmix64) = s.fork

        override def onZip[A, B, C](aa: (A, Splitmix64), bb: (B, Splitmix64), k: (A, B) => C) =
          val (a, _) = aa
          val (b, s) = bb
          (k(a, b), s)

        private inline def simple[A](inline f: Splitmix64 => A): A !! ThisEffect =
          Local.update: s =>
            val s2 = s.next
            (f(s2), s2)

        override def nextBoolean: Boolean !! ThisEffect = simple(x => (x.value & 1) != 0)
        override def nextInt: Int !! ThisEffect = simple(_.value.toInt)
        override def nextInt(n: Int): Int !! ThisEffect = between(0, n)
        override def nextLong: Long !! ThisEffect = simple(_.value)
        override def nextLong(n: Long): Long !! ThisEffect = between(0, n)
        override def nextFloat: Float !! ThisEffect = simple(_.toDoubleInclusive.toFloat)
        override def nextDouble: Double !! ThisEffect = simple(_.toDoubleInclusive)

        private inline def between[A](range: Double, inline f: Double => A): A !! ThisEffect =
          simple(x => f((x.toDoubleExclusive * range).floor))

        override def between(minInclusive: Long, maxExclusive: Long): Long !! ThisEffect =
          between((maxExclusive - minInclusive).toDouble, _.floor.toLong + minInclusive)

        override def between(minInclusive: Int, maxExclusive: Int): Int !! ThisEffect =
          between(maxExclusive - minInclusive, _.floor.toInt + minInclusive)

        override def between(minInclusive: Double, maxExclusive: Double): Double !! ThisEffect =
          between(maxExclusive - minInclusive, _.floor + minInclusive)

        override def between(minInclusive: Float, maxExclusive: Float): Float !! ThisEffect =
          between(maxExclusive - minInclusive, _.floor.toFloat + minInclusive)

        override def nextGaussian: Double !! ThisEffect = Local.update(_.gaussian)
        override def nextBytes(n: Int): Array[Byte] !! ThisEffect = Local.update(_.bytes(n))
        override def setSeed(seed: Long): Unit !! ThisEffect = Local.modify(_.seed(seed))
      .toHandler
      .dropState


    /** Non deterministic. */
    def shared(seed: Long): Handler[Identity, Identity, enclosing.type, IO] =
      IO(new ScalaRandom(seed)).flatMapHandler: rng =>
        new impl.Proxy[IO] with RandomSignature:
          override def nextBoolean: Boolean !! ThisEffect = IO(rng.nextBoolean)
          override def nextInt: Int !! ThisEffect = IO(rng.nextInt)
          override def nextInt(n: Int): Int !! ThisEffect = IO(rng.nextInt(n))
          override def nextLong: Long !! ThisEffect = IO(rng.nextLong)
          override def nextLong(n: Long): Long !! ThisEffect = IO(rng.nextLong(n))
          override def nextFloat: Float !! ThisEffect = IO(rng.nextFloat)
          override def nextDouble: Double !! ThisEffect = IO(rng.nextDouble)
          override def nextGaussian: Double !! ThisEffect = IO(rng.nextGaussian)
          override def between(minInclusive: Int, maxExclusive: Int): Int !! ThisEffect = IO(rng.between(minInclusive, maxExclusive))
          override def between(minInclusive: Long, maxExclusive: Long): Long !! ThisEffect = IO(rng.between(minInclusive, maxExclusive))
          override def between(minInclusive: Float, maxExclusive: Float): Float !! ThisEffect = IO(rng.between(minInclusive, maxExclusive))
          override def between(minInclusive: Double, maxExclusive: Double): Double !! ThisEffect = IO(rng.between(minInclusive, maxExclusive))
          override def nextBytes(n: Int): Array[Byte] !! ThisEffect = IO(rng.nextBytes(n))
          override def setSeed(seed: Long): Unit !! ThisEffect = IO(rng.setSeed(seed))
        .toHandler


object RandomEffect:
  extension (thiz: RandomEffect)
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler: Handler[Identity, Identity, thiz.type, IO] = thiz.handlers.local


/** Predefined instance of this [[RandomEffect]]. */
case object Random extends RandomEffect
type Random = Random.type
