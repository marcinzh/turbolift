package turbolift.type_safety
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Dummies._


class EffectSubtypingTest extends AnyFlatSpec:
  "Effect subtyping" should "work" in {
    assertCompiles {"implicitly[Eff12 <:< Eff123]"}
    assertTypeError {"implicitly[Eff12 <:< Eff1]"}
  }

