package turbolift.operations
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._


class NonDetTest extends Specification {
  def is = List(wog, wg).reduce(_ and _)

  def wog = {
    case object Fx extends NonDet

    val eff = for {
      i <- Fx.each(1 to 2)
      c <- Fx.each('a' to 'b')
    } yield s"$i$c"

    eff.runWith(Fx.handler) must_== Vector("1a", "1b", "2a", "2b"),
  }


  def wg = {
    case object Fx extends NonDet

    val eff = for {
      i <- Fx.each(0 to 3)
      if i % 2 != 0
      c <- Fx.each('a' to 'c')
    } yield s"$i$c"

    eff.runWith(Fx.handler) must_== Vector("1a", "1b", "1c", "3a", "3b", "3c"),
  }
}
