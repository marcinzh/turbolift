package turbolift.std_effects
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!
import turbolift.std_effects.State


class StateTests extends AnyFunSpec:
  describe("Basic ops") {
    it("get") {
      case object Fx extends State[Int]
      Fx.get
      .runWith(Fx.handler(1)) shouldEqual ((1, 1))
    }

    it("put") {
      case object Fx extends State[Int]
      Fx.put(2)
      .runWith(Fx.handler(1)) shouldEqual (((), 2))
    }

    it("modify") {
      case object Fx extends State[Int]
      Fx.modify(_ + 10)
      .runWith(Fx.handler(1)) shouldEqual (((), 11))
    }
  }

  describe("Combined ops") {
    it("put & get") {
      case object Fx extends State[Int]
      (for
        a <- Fx.get
        _ <- Fx.put(2)
        b <- Fx.get
      yield (a, b))
      .runWith(Fx.handler(1)) shouldEqual (((1, 2), 2))
    }
      
    it("2 states interleaved") {
      case object Fx1 extends State[Int]
      case object Fx2 extends State[Int]
      (for
        a <- Fx1.get
        b <- Fx2.get
        _ <- Fx1.modify(_ * 10)
        _ <- Fx2.modify(_ * 10)
        _ <- Fx1.modify(_ + b)
        _ <- Fx2.modify(_ + a)
      yield (a, b))
      .runWith(Fx1.handler(1) ***! Fx2.handler(2)) shouldEqual (((1, 2), (12, 21)))
    }
  }
