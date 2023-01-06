package turbolift.effects.default_handlers
import scala.util.{Random => ScalaRandom}
import turbolift.!!
import turbolift.io.IO
import turbolift.effects.{RandomEffect, RandomSig}


extension (fx: RandomEffect)
  private[effects] def randomHandler_shared(seed: Long): fx.ThisHandler.Id[IO] =
    IO(new ScalaRandom(seed)) >>=! { rng =>
      new fx.Proxy[IO] with RandomSig:
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
        override def setSeed(seed: Long): Unit !@! ThisEffect = IO(rng.setSeed(seed))
      .toHandler
    }

  private[effects] def randomHandler_shared: fx.ThisHandler.Id[IO] =
    IO(ScalaRandom.nextLong) >>=! (randomHandler_shared(_))
