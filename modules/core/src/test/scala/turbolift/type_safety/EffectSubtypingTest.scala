package turbolift.type_safety
import org.specs2._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import Dummies._


class EffectSubtypingTest2 extends Specification:
  def is = br ^ "Effect subtyping" ! (good and bad)
  def good = Typecheck {"implicitly[Eff12 <:< Eff123]"} must succeed
  def bad  = Typecheck {"implicitly[Eff12 <:< Eff1]"} must succeed.not
