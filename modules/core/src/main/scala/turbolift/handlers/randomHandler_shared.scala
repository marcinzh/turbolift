package turbolift.handlers
import scala.util.{Random => ScalaRandom}
import turbolift.!!
import turbolift.effects.{RandomEffect, RandomSignature, IO}


extension (fx: RandomEffect)
  def randomHandler_shared(seed: Long): fx.ThisHandler.FromId.ToId[IO] =
    IO(new ScalaRandom(seed)) >>=! { rng =>
      new fx.impl.Proxy[IO] with RandomSignature:
        override def nextBoolean: Boolean !@! ThisEffect = IO(rng.nextBoolean)
        override def nextInt: Int !@! ThisEffect = IO(rng.nextInt)
        override def nextInt(n: Int): Int !@! ThisEffect = IO(rng.nextInt(n))
        override def nextLong: Long !@! ThisEffect = IO(rng.nextLong)
        override def nextLong(n: Long): Long !@! ThisEffect = IO(rng.nextLong(n))
        override def nextFloat: Float !@! ThisEffect = IO(rng.nextFloat)
        override def nextDouble: Double !@! ThisEffect = IO(rng.nextDouble)
        override def nextGaussian: Double !@! ThisEffect = IO(rng.nextGaussian)
        override def between(minInclusive: Int, maxExclusive: Int): Int !@! ThisEffect = IO(rng.between(minInclusive, maxExclusive))
        override def between(minInclusive: Long, maxExclusive: Long): Long !@! ThisEffect = IO(rng.between(minInclusive, maxExclusive))
        override def between(minInclusive: Float, maxExclusive: Float): Float !@! ThisEffect = IO(rng.between(minInclusive, maxExclusive))
        override def between(minInclusive: Double, maxExclusive: Double): Double !@! ThisEffect = IO(rng.between(minInclusive, maxExclusive))
        override def nextBytes(n: Int): Array[Byte] !@! ThisEffect = IO(rng.nextBytes(n))
        override def setSeed(seed: Long): Unit !@! ThisEffect = IO(rng.setSeed(seed))
      .toHandler
    }

  def randomHandler_shared: fx.ThisHandler.FromId.ToId[IO] =
    IO(ScalaRandom.nextLong) >>=! (randomHandler_shared(_))
