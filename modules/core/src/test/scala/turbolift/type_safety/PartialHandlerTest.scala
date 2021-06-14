package turbolift.type_safety
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Dummies._


class PartialHandlerTest extends AnyFlatSpec:
  "Partial handlers" should "not leak effects" in {
    assertCompiles {"""
      any[Eff123]
      .handleWith[Fx2.type with Fx3.type](any[H1])
      .handleWith[Fx3.type](any[H2])
      .handleWith[Any](any[H3])
      .run
    """}

    assertCompiles {"""
      any[Eff123]
      .handleWith[Fx3.type](any[H21])
      .handleWith[Any](any[H3])
      .run
    """}

    assertCompiles {"""
      any[Eff123]
      .handleWith[Any](any[H321])
      .run
    """}

    assertTypeError {"""
      any[Eff12]
      .handleWith[Fx3.type](any[H1])
    """}

    assertTypeError {"""
      any[Eff12]
      .handleWith[Fx1.type](any[H3])
    """}
  }
