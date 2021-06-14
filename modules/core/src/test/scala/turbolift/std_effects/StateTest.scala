package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!
import turbolift.std_effects.State


class StateTests extends AnyFlatSpec:

  "State operations" should "work" in {
    case object Fx extends State[Int]

    Fx.get.runWith(Fx.handler(42).eval) shouldEqual 42

    Fx.put(1337).flatMap(_ => Fx.put(42)).runWith(Fx.handler(-1).exec) shouldEqual 42

    Fx.put(42).flatMap(_ => Fx.get).runWith(Fx.handler(-1)) shouldEqual (42, 42)
    
    Fx.modify(_ * 10).runWith(Fx.handler(42).exec) shouldEqual 420
  }

  "Multiple State operations" should "work" in {
    case object Fx1 extends State[Int]
    case object Fx2 extends State[Int]

    (for
      a <- Fx1.get
      _ <- Fx2.put(a * 10)
      b <- Fx1.get
    yield b)
    .runWith(Fx1.handler(42) ***! Fx2.handler(1337)) shouldEqual ((42, 420), 42)
  }
