package turbolift.type_safety
import org.specs2._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import Dummies._


class PartialHandlerTest extends Specification:
  def is = br ^ "Partial handlers should not leak effects" !
    List(good111, good21, good3, bad1, bad2).reduce(_ and _)


  def good111 = Typecheck {"""
    any[Eff123]
    .handleWith[Fx2 & Fx3](any[H1])
    .handleWith[Fx3](any[H2])
    .handleWith[Any](any[H3])
    .run
  """} must succeed 

  def good21 = Typecheck {"""
    any[Eff123]
    .handleWith[Fx3](any[H21])
    .handleWith[Any](any[H3])
    .run
  """} must succeed 

  def good3 = Typecheck {"""
    any[Eff123]
    .handleWith[Any](any[H321])
    .run
  """} must succeed 

  def bad1 = Typecheck {"""
    any[Eff12]
    .handleWith[Fx3](any[H1])
  """} must succeed.not 

  def bad2 = Typecheck {"""
    any[Eff12]
    .handleWith[Fx1](any[H3])
  """} must succeed.not
