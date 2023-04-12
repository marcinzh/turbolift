package turbolift.effects
import turbolift.{!!, Signature, Effect}
import turbolift.io.IO
import turbolift.effects.default_handlers.{randomHandler_local, randomHandler_shared}


trait RandomSignature extends Signature:
  def nextBoolean: Boolean !@! ThisEffect
  def nextInt: Int !@! ThisEffect
  def nextInt(n: Int): Int !@! ThisEffect
  def nextLong: Long !@! ThisEffect
  def nextLong(n: Long): Long !@! ThisEffect
  def nextFloat: Float !@! ThisEffect
  def nextDouble: Double !@! ThisEffect
  def nextGaussian: Double !@! ThisEffect
  def between(minInclusive: Int, maxExclusive: Int): Int !@! ThisEffect
  def between(minInclusive: Long, maxExclusive: Long): Long !@! ThisEffect
  def between(minInclusive: Float, maxExclusive: Float): Float !@! ThisEffect
  def between(minInclusive: Double, maxExclusive: Double): Double !@! ThisEffect
  def nextBytes(n: Int): Array[Byte] !@! ThisEffect
  def setSeed(seed: Long): Unit !@! ThisEffect


trait RandomEffect extends Effect[RandomSignature] with RandomSignature:
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
  final override def nextBytes(n: Int): Array[Byte] !@! ThisEffect = perform(_.nextBytes(n))
  final override def setSeed(seed: Long): Unit !! this.type = perform(_.setSeed(seed))

  /** Default handler for this effect. */
  def handler: ThisHandler.Id[IO] = handlers.shared

  /** Default handler for this effect. */
  def handler(seed: Long): ThisHandler.Id[IO] = handlers.shared(seed)

  /** Predefined handlers for this effect. */
  object handlers:
    def local: ThisHandler.Id[IO] = RandomEffect.this.randomHandler_local
    def shared: ThisHandler.Id[IO] = RandomEffect.this.randomHandler_shared
    def local(seed: Long): ThisHandler.FreeId = RandomEffect.this.randomHandler_local(seed)
    def shared(seed: Long): ThisHandler.Id[IO] = RandomEffect.this.randomHandler_shared(seed)


/** Predefined instance of this effect. */
case object Random extends RandomEffect
type Random = Random.type
