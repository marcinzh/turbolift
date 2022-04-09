package turbolift.type_safety
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Dummies._


class EffectSubtypingTest extends AnyFunSpec:
  describe("Effect subtyping") {
    assertCompiles {"implicitly[Eff12 <:< Eff123]"}
    assertTypeError {"implicitly[Eff12 <:< Eff1]"}
  }

