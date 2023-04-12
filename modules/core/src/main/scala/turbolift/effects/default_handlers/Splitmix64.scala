package turbolift.effects.default_handlers
import java.lang.StrictMath
import Double.NaN
import Splitmix64._


private[effects] case class Splitmix64(value: Long, secondGaussian: Double = NaN):
  def next: Splitmix64 = copy(value = mix1(value))
  def jump: Splitmix64 = Splitmix64(mix2(value))
  def seed(n: Long): Splitmix64 = copy(value = n ^ S)

  def toDoubleInclusive: Double = toDoubleExclusive //@#@TODO
  def toDoubleExclusive: Double = f01(value)

  def gaussian: (Double, Splitmix64) =
    if secondGaussian != NaN then
      (secondGaussian, copy(secondGaussian = NaN))
    else
      var t = value
      var s, x, y = 0.0
      while
        val x = f11(t)
        t = mix(t)
        val y = f11(t)
        t = mix(t)
        s = x * x + y * y;
        s >= 1 || s == 0
      do ()
      val m = StrictMath.sqrt(-2 * StrictMath.log(s) / s)
      (m * x, Splitmix64(t, m * y))

  def bytes(size: Int): (Array[Byte], Splitmix64) =
    val arr = new Array[Byte](size)
    def loop(i: Int, t: Long): Long =
      if i < size then
        val j = i & 7
        val t2 = if j == 0 then mix1(t) else t
        arr(i) = (t2 >>> (j * 8)).toByte
        loop(i + 1, t2)
      else
        t
    val value2 = loop(0, value)
    (arr, Splitmix64(value2))


private object Splitmix64:
  private inline val S = 6371467827229002779L
  private inline val G1 = 0x9e3779b97f4a7c15L
  private inline val G2 = G1 * 0x1337c0d3
  private inline val D = 1L << 53

  private def mix1(a: Long): Long = mix(a + G1)
  private def mix2(a: Long): Long = mix(a + G2)

  private def mix(a: Long): Long =
    val b = (a ^ (a >>> 30)) * 0xbf58476d1ce4e5b9L
    val c = (b ^ (b >>> 27)) * 0x94d049bb133111ebL
    c ^ (c >>> 31)

  private def f01(a: Long): Double = (a & (D - 1)).toDouble / D
  private def f11(a: Long): Double = f01(a) * 2 - 1
