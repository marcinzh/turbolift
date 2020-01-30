package turbolift.type_safety
// import turbolift.abstraction.!!
import org.specs2._
import org.specs2.execute._, Typecheck._
import org.specs2.matcher.TypecheckMatchers._
import Dummies._


class EffectSubtypingTest extends Specification {
  def is = br ^ "Effect subtyping" ! (good and bad)
  def good = typecheck {"implicitly[Eff12 <:< Eff123]"} must succeed
  def bad  = typecheck {"implicitly[Eff12 <:< Eff1]"} must not succeed
}
