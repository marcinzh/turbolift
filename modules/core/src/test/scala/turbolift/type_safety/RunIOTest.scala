package turbolift.type_safety
import org.specs2._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.Warp


class RunIOTest extends Specification:
  def is = br ^ ".runIO requires at most IO & Warp effects" ! {
    case object Fx
    type Fx = Fx.type
    val name = "foo"
    def good1 : Unit !! Any = ???
    def good2 : Unit !! IO = ???
    def good3 : Unit !! Warp = ???
    def good4 : Unit !! (Warp & IO) = ???
    def good5 : Unit !! (IO & Warp) = ???
    def bad1 : Unit !! Fx = ???
    def bad2 : Unit !! (IO & Warp & Fx) = ???


    List(
      Typecheck {"good1.runIO"} must succeed,
      Typecheck {"good2.runIO"} must succeed,
      Typecheck {"good3.runIO"} must succeed,
      Typecheck {"good4.runIO"} must succeed,
      Typecheck {"good5.runIO"} must succeed,
      Typecheck {"bad1.runIO"} must succeed.not,
      Typecheck {"bad2.runIO"} must succeed.not,
      Typecheck {"good1.named(name).runIO"} must succeed,
      Typecheck {"good2.named(name).runIO"} must succeed,
      Typecheck {"good3.named(name).runIO"} must succeed,
      Typecheck {"good4.named(name).runIO"} must succeed,
      Typecheck {"good5.named(name).runIO"} must succeed,
      Typecheck {"bad1.named(name).runIO"} must succeed.not,
      Typecheck {"bad2.named(name).runIO"} must succeed.not,
    )
  }