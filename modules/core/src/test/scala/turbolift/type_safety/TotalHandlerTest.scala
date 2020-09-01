package turbolift.type_safety
import turbolift.abstraction._
import org.specs2._
import org.specs2.execute._, Typecheck._
import org.specs2.matcher.TypecheckMatchers._
import Dummies._


class TotalHandlerTest extends Specification {
  type H12 = H1 <<<! H2
  type H21 = H2 <<<! H1

  def is = br ^ "Total handler's effects should be superset of handled computation's effects" ! List(
    typecheck {"any[H12] run any[Eff1]"}   must succeed,
    typecheck {"any[H12] run any[Eff2]"}   must succeed,
    typecheck {"any[H12] run any[Eff12]"}  must succeed,
    typecheck {"any[H12] run any[Eff3]"}   must not succeed,
    typecheck {"any[H12] run any[Eff123]"} must not succeed,
    typecheck {"any[H21] run any[Eff1]"}   must succeed,
    typecheck {"any[H21] run any[Eff2]"}   must succeed,
    typecheck {"any[H21] run any[Eff12]"}  must succeed,
    typecheck {"any[H21] run any[Eff3]"}   must not succeed,
    typecheck {"any[H21] run any[Eff123]"} must not succeed,

    typecheck {"any[Eff1] runWith any[H12]"}   must succeed,
    typecheck {"any[Eff2] runWith any[H12]"}   must succeed,
    typecheck {"any[Eff12] runWith any[H12]"}  must succeed,
    typecheck {"any[Eff3] runWith any[H12]"}   must not succeed,
    typecheck {"any[Eff123] runWith any[H12]"} must not succeed,
    typecheck {"any[Eff1] runWith any[H21]"}   must succeed,
    typecheck {"any[Eff2] runWith any[H21]"}   must succeed,
    typecheck {"any[Eff12] runWith any[H21]"}  must succeed,
    typecheck {"any[Eff3] runWith any[H21]"}   must not succeed,
    typecheck {"any[Eff123] runWith any[H21]"} must not succeed
  ).reduce(_ and _)
}
