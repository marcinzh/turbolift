package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!
import turbolift.std_effects.{Validation, Writer}


class ValidationTest extends AnyFlatSpec with CanLaunchTheMissiles:
  "raise" should "work" in {
    case object Fx extends Validation[String]

    val missile1 = Missile()
    val missile2 = Missile()
    (for
      _ <- Fx.raise("x") *! missile1.launch_! *! Fx.raise("y") *! Fx.raise("z")
      _ <- missile2.launch_!
      _ <- Fx.raise("w")
    yield ())
    .runWith(Fx.handler) shouldEqual Left("xyz")
    missile1.mustHaveLaunchedOnce
    missile2.mustNotHaveLaunched
  }

  "katch" should "work" in {
    case object FxV extends Validation[String]
    case object FxW extends Writer[Int]

    def mkEff =
      val missile1 = Missile()
      val missile2 = Missile()
      val comp = FxV.katch {
        for
          _ <- FxW.tell(42)
          _ <- FxV.raise("x") *! missile1.launch_! *! FxV.raise("y") *! FxV.raise("z")
          _ <- FxV.raise("w")
          _ <- FxW.tell(1337)
          _ <- missile2.launch_!
        yield "???"
      } (str => !!.pure(str.toUpperCase))
      (comp, missile1, missile2)

    {
      val (comp, missile1, missile2) = mkEff
      val result = comp.runWith(FxV.handler <<<! FxW.handler)
      result shouldEqual Right((0, "XYZ"))
      missile1.mustHaveLaunchedOnce
      missile2.mustNotHaveLaunched
    }

    {
      val (comp, missile1, missile2) = mkEff
      val result = comp.runWith(FxW.handler <<<! FxV.handler)
      result shouldEqual ((42, Right("XYZ")))
      missile1.mustHaveLaunchedOnce
      missile2.mustNotHaveLaunched
    }

    // br ^ "validate: Validation <<<! Writer" ! testVS ^
    // br ^ "validate: Writer <<<! Validation" ! testSV
  }
