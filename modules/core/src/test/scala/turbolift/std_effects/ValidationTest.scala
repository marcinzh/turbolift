package turbolift.operations
import turbolift.abstraction.!!
import turbolift.std_effects.{Validation, Writer}
import cats.implicits._
import org.specs2._
// import mwords._


class ValidationTest extends Specification with CanLaunchTheMissiles {
  def is = List(raise, validate).reduce(_ ^ _)

  def raise = br ^ "raise" ! {
    case object Fx extends Validation[String]

    val missile1 = Missile()
    val missile2 = Missile()
    (for {
      _ <- Fx.raise("x") *! missile1.launch_! *! Fx.raise("y") *! Fx.raise("z")
      _ <- missile2.launch_!
      _ <- Fx.raise("w")
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
      val comp = FxV.katch {
        for {
          _ <- FxW.tell(42)
          _ <- FxV.raise("x") *! missile1.launch_! *! FxV.raise("y") *! FxV.raise("z")
          _ <- FxV.raise("w")
          _ <- FxW.tell(1337)
          _ <- missile2.launch_!
        } yield "???"
      } (str => !!.pure(str.toUpperCase))
      (comp, missile1, missile2)
    }

    val testVS = {
      val (comp, missile1, missile2) = mkEff
      val result = comp.runWith(FxV.handler <<<! FxW.handler)
      result must_== Right((0, "XYZ")) and
      missile1.mustHaveLaunchedOnce and
      missile2.mustNotHaveLaunched
    }

    val testSV = {
      val (comp, missile1, missile2) = mkEff
      val result = comp.runWith(FxW.handler <<<! FxV.handler)
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
