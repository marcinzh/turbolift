package turbolift.type_safety
import org.specs2._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import Dummies._


class TotalHandlerTest extends Specification:
  def is = br ^ "Total handler's effects should be superset of handled computation's effects" !
    List(
      Typecheck {"any[Eff1]   .handleWith(any[H12]).run"} must succeed,
      Typecheck {"any[Eff2]   .handleWith(any[H12]).run"} must succeed,
      Typecheck {"any[Eff12]  .handleWith(any[H12]).run"} must succeed,
      Typecheck {"any[Eff3]   .handleWith(any[H12]).run"} must succeed.not,
      Typecheck {"any[Eff123] .handleWith(any[H12]).run"} must succeed.not,
      Typecheck {"any[Eff1]   .handleWith(any[H21]).run"} must succeed,
      Typecheck {"any[Eff2]   .handleWith(any[H21]).run"} must succeed,
      Typecheck {"any[Eff12]  .handleWith(any[H21]).run"} must succeed,
      Typecheck {"any[Eff3]   .handleWith(any[H21]).run"} must succeed.not,
      Typecheck {"any[Eff123] .handleWith(any[H21]).run"} must succeed.not,
    )
