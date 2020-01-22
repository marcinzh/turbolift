package turbolift.operations
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._
// import mwords._


class ValidationTest extends Specification with CanLaunchTheMissiles {
  def is = List(invalid, validate).reduce(_ ^ _)

  def invalid = br ^ "invalid" ! {
    case object Fx extends Validation[String]

    val missile1 = Missile()
    val missile2 = Missile()
    (for {
      _ <- Fx.invalid("x") *! missile1.launch_! *! Fx.invalid("y") *! Fx.invalid("z")
      _ <- missile2.launch_!
      _ <- Fx.invalid("w")
    } yield ())
    .runWith(Fx.handler) must_== Left("xyz") and
    missile1.mustHaveLaunchedOnce and
    missile2.mustNotHaveLaunched
  }

  def validate = {
    case object FxV extends Validation[String]
    case object FxW extends Writer[Int]

    def mkEff = {
      val missile1 = Missile()
      val missile2 = Missile()
      val eff = FxV.validate {
        for {
          _ <- FxW.tell(42)
          _ <- FxV.invalid("x") *! missile1.launch_! *! FxV.invalid("y") *! FxV.invalid("z")
          _ <- FxV.invalid("w")
          _ <- FxW.tell(1337)
          _ <- missile2.launch_!
        } yield "???"
      } (str => Return(str.toUpperCase))
      (eff, missile1, missile2)
    }

    val testVS = {
      val (eff, missile1, missile2) = mkEff
      val result = eff.runWith(FxV.handler <<<! FxW.handler)
      result must_== Right((0, "XYZ")) and
      missile1.mustHaveLaunchedOnce and
      missile2.mustNotHaveLaunched
    }

    val testSV = {
      val (eff, missile1, missile2) = mkEff
      val result = eff.runWith(FxW.handler <<<! FxV.handler)
      result must_== ((42, Right("XYZ"))) and
      missile1.mustHaveLaunchedOnce and
      missile2.mustNotHaveLaunched
    }

    br ^ "validate: Validation <<<! Writer" ! testVS ^
    br ^ "validate: Writer <<<! Validation" ! testSV

    // br ^ "Writer <<<! Validation" ! testSV ^
    // br ^ "Validation <<<! Writer" ! testVS
  }
}
