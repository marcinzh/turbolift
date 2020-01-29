package turbolift.operations
import turbolift.abstraction.!!
import turbolift.abstraction.implicits._
import turbolift.std_effects.{Choice, Except}
import org.specs2._


class ChoiceTest extends Specification {
  def is = List(withoutGuard, withGuard, withExcept).reduce(_ ^ _)

  def withoutGuard = br ^ "each: without guard" ! {
    case object Fx extends Choice

    val eff = for {
      i <- Fx.each(1 to 2)
      c <- Fx.each('a' to 'b')
    } yield s"$i$c"

    eff.runWith(Fx.handler) must_== Vector("1a", "1b", "2a", "2b"),
  }


  def withGuard = br ^ "each: with guard" ! {
    case object Fx extends Choice

    val eff = for {
      i <- Fx.each(0 to 3)
      if i % 2 != 0
      c <- Fx.each('a' to 'c')
    } yield s"$i$c"

    eff.runWith(Fx.handler) must_== Vector("1a", "1b", "1c", "3a", "3b", "3c"),
  }


  def withExcept = {
    case object FxC extends Choice
    case object FxE extends Except[Int]

    val eff = !!.pure(1) +! FxE.raise(2)

    def testCE = eff.runWith(FxC.handler <<<! FxE.handler) must_== Vector(Right(1), Left(2))
    def testEC = eff.runWith(FxE.handler <<<! FxC.handler) must_== Left(2)

    br ^ "each: Choice <<<! Except" ! testCE ^
    br ^ "each: Except <<<! Choice" ! testEC
  }
}
