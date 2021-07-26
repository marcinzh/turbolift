package turbolift.type_safety
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Dummies._


class TotalHandlerTest extends AnyFlatSpec:
  "Total handlers" should "not leak effects" in {
    assertCompiles  {"any[H12] run any[Eff1]"}
    assertCompiles  {"any[H12] run any[Eff2]"}
    assertCompiles  {"any[H12] run any[Eff12]"}
    assertTypeError {"any[H12] run any[Eff3]"}
    assertTypeError {"any[H12] run any[Eff123]"}
    assertCompiles  {"any[H21] run any[Eff1]"}
    assertCompiles  {"any[H21] run any[Eff2]"}
    assertCompiles  {"any[H21] run any[Eff12]"}
    assertTypeError {"any[H21] run any[Eff3]"}
    assertTypeError {"any[H21] run any[Eff123]"}

    assertCompiles  {"any[Eff1] runWith any[H12]"}
    assertCompiles  {"any[Eff2] runWith any[H12]"}
    assertCompiles  {"any[Eff12] runWith any[H12]"}
    assertTypeError {"any[Eff3] runWith any[H12]"}
    assertTypeError {"any[Eff123] runWith any[H12]"}
    assertCompiles  {"any[Eff1] runWith any[H21]"}
    assertCompiles  {"any[Eff2] runWith any[H21]"}
    assertCompiles  {"any[Eff12] runWith any[H21]"}
    assertTypeError {"any[Eff3] runWith any[H21]"}
    assertTypeError {"any[Eff123] runWith any[H21]"}
  }
