package turbolift.std_effects
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.Assertion
import turbolift.{!!, Handler}
import turbolift.typeclass.Accum
import turbolift.std_effects.{Except, State}


class ExceptTest extends AnyFunSpec with CanLaunchTheMissiles:
  private class Picker(round: Boolean):
    def apply[T](a: => T)(b: => T): T = if round then a else b
    def name = apply("one")("many")
    def handler[T, Fx <: Except[T]](fx: Fx)(using Accum[T, T]): fx.ThisHandler.Free[Either[T, *]] = apply(fx.handlers.one)(fx.handlers.many)

  private object Picker:
    def foreach(f: Picker => Unit): Unit =
      for round <- List(true, false) do
        f(Picker(round))

  describe("Basic ops") {
    it("raise") {
      case object Fx extends Except[Int]
      val missile = Missile()
      (Fx.raise(1) &&! missile.launch_!)
      .runWith(Fx.handler) shouldEqual Left(1)
      missile.mustNotHaveLaunched
    }

    it("katch") {
      case object Fx extends Except[Int]
      Fx.katch(Fx.raise(1))(_ => !!.pure(2))
      .runWith(Fx.handler) shouldEqual Right(2)
    }
  }

  describe("Combined ops") {
    describe("katch & State") {
      case object FxE extends Except[String]
      case object FxS extends State[Int]
      val comp =
        FxE.katch {
          for
            _ <- FxS.put(1)
            _ <- FxE.raise("e")
            _ <- FxS.put(2)
          yield true
        } (_ => !!.pure(false))

      for picker <- Picker do
        val hS = FxS.handler(0)
        val hE = picker.handler(FxE)
        describe("With handler = " + picker.name) {
          it("State before Except") {
            comp.runWith(hS &&&! hE) shouldEqual Right((false, 0))
          }

          it("Except before State") {
            comp.runWith(hE &&&! hS) shouldEqual ((Right(false), 1))
          }
        }
    }
  }


  describe("Par ops") {
    for picker <- Picker do
      describe("With handler = " + picker.name) {
        it("raise") {
          case object Fx extends Except[Int]
          val missile = Missile()
          (Fx.raise(1) *! missile.launch_! *! Fx.raise(2))
          .runWith(picker.handler(Fx)) shouldEqual Left(picker(1)(3))
          missile.mustHaveLaunchedOnce
          //@#@TODO io
          // picker(missile.mustNotHaveLaunched)(missile.mustHaveLaunchedOnce)
        }

        describe("katch & Writer") {
          case object FxE extends Except[String]
          case object FxW extends Writer[String]
          val comp = 
            FxE.katch {
            for
              _ <- FxW.tell("a")
              _ <- FxW.tell("b") *! FxE.raise("x") *! FxW.tell("c") *! FxE.raise("y")
              _ <- FxW.tell("d")
              _ <- FxE.raise("z")
            yield "?"
          } (str => !!.pure(str.toUpperCase))

          val hE = picker.handler(FxE)
          val hW = FxW.handler

          it("Writer before Except") {
            val err = picker("X")("XY")
            val acc = picker("")("")
            comp.runWith(hW &&&! hE) shouldEqual Right((err, acc))
          }

          it("Except before Writer") {
            val err = picker("X")("XY")
            val acc = picker("abc")("abc")
            comp.runWith(hE &&&! hW) shouldEqual ((Right(err), acc))
          }
        }
      }
  }
