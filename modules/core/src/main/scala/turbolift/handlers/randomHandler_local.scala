package turbolift.handlers
import scala.util.{Random => ScalaRandom}
import turbolift.!!
import turbolift.effects.{RandomEffect, RandomSignature, IO}


extension (fx: RandomEffect)
  def randomHandler_local(seed: Long): fx.ThisHandler.FromId.ToId.Free =
    new fx.impl.Stateful.FromId.Free[(_, Splitmix64)] with fx.impl.Parallel.ForkJoin with RandomSignature:
      override type Stan = Splitmix64

      override def tailResumptiveHint: Boolean = true

      override def onInitial: Stan !! Any = !!.pure(Splitmix64(seed))

      override def onReturn(a: Unknown, s: Splitmix64): (Unknown, Splitmix64) !! Any = !!.pure((a, s))

      override def onRestart(aa: (Unknown, Splitmix64)) =
        val (a, s) = aa
        fx.setSeed(s.value) &&! !!.pure(a)

      override def onFork(s: Stan): (Stan, Stan) =
        val s2 = s.next
        val s3 = s.jump
        (s2, s3)

      override def onZip[A, B, C](aa: (A, Splitmix64), bb: (B, Splitmix64), k: (A, B) => C) =
        val (a, _) = aa
        val (b, s) = bb
        (k(a, b), s)

      inline def simple[A](inline f: Splitmix64 => A): A !@! ThisEffect =
        (k, s) =>
          val s2 = s.next
          k(f(s2), s2)

      override def nextBoolean: Boolean !@! ThisEffect = simple(x => (x.value & 1) != 0)
      override def nextInt: Int !@! ThisEffect = simple(_.value.toInt)
      override def nextInt(n: Int): Int !@! ThisEffect = between(0, n)
      override def nextLong: Long !@! ThisEffect = simple(_.value)
      override def nextLong(n: Long): Long !@! ThisEffect = between(0, n)
      override def nextFloat: Float !@! ThisEffect = simple(_.toDoubleInclusive.toFloat)
      override def nextDouble: Double !@! ThisEffect = simple(_.toDoubleInclusive)

      inline def between[A](range: Double, inline f: Double => A): A !@! ThisEffect =
        simple(x => f((x.toDoubleExclusive * range).floor))

      override def between(minInclusive: Long, maxExclusive: Long): Long !@! ThisEffect =
        between((maxExclusive - minInclusive).toDouble, _.floor.toLong + minInclusive)

      override def between(minInclusive: Int, maxExclusive: Int): Int !@! ThisEffect =
        between(maxExclusive - minInclusive, _.floor.toInt + minInclusive)

      override def between(minInclusive: Double, maxExclusive: Double): Double !@! ThisEffect =
        between(maxExclusive - minInclusive, _.floor + minInclusive)

      override def between(minInclusive: Float, maxExclusive: Float): Float !@! ThisEffect =
        between(maxExclusive - minInclusive, _.floor.toFloat + minInclusive)

      override def nextGaussian: Double !@! ThisEffect = (k, s) => k.tupled(s.gaussian)
      override def nextBytes(n: Int): Array[Byte] !@! ThisEffect = (k, s) => k.tupled(s.bytes(n))
      override def setSeed(seed: Long): Unit !@! ThisEffect = (k, s) => k((), s.seed(seed))

    .toHandler
    .dropState


  def randomHandler_local: fx.ThisHandler.FromId.ToId[IO] =
    IO(ScalaRandom.nextLong) >>=! (randomHandler_local(_))
