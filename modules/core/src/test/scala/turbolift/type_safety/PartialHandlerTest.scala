package turbolift.type_safety
import turbolift.abstraction._
import turbolift.abstraction.implicits._
import org.specs2._
import org.specs2.execute._, Typecheck._
import org.specs2.matcher.TypecheckMatchers._
import Dummies._


class PartialHandlerTest extends Specification {
  def is = (good111 and good21 and good3 and bad1 and bad2)

  def good111 = typecheck {"""
    any[Eff123]
    .handleWith[Fx2.type with Fx3.type](any[H1])
    .handleWith[Fx3.type](any[H2])
    .handleWith[Any](any[H3])
    .run
  """} must succeed	

  def good21 = typecheck {"""
    any[Eff123]
    .handleWith[Fx3.type](any[H1 <<<! H2])
    .handleWith[Any](any[H3])
    .run
  """} must succeed	

  def good3 = typecheck {"""
    any[Eff123]
    .handleWith[Any](any[H1 <<<! H2 <<<! H3])
    .run
  """} must succeed	

  def bad1 = typecheck {"""
    any[Eff12]
    .handleWith[Fx3.type](any[H1])
  """} must not succeed	

  def bad2 = typecheck {"""
    any[Eff12]
    .handleWith[Fx1.type](any[H3])
  """} must not succeed	
}
