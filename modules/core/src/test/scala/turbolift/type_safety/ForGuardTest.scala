package turbolift.type_safety
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._
import org.specs2.execute._, Typecheck._
import org.specs2.matcher.TypecheckMatchers._


class ForGuardTest extends Specification {
  case object FxF extends Maybe
  case object FxN extends Reader[Int]

  val nonFilterableEff = FxN.ask
  val filterableEff = FxF.from(Some(1))

  def is = List(ni, fi, nfi, fni, nfpi, fnpi, nif, fin).reduce(_ and _)

  def ni = typecheck {"""
    (for {
      a <- nonFilterableEff
      if a > 0
    } yield a)
  """} must not succeed

  def fi = typecheck {"""
    (for {
      b <- filterableEff
      if b > 0
    } yield b)
  """} must succeed

  def nfi = typecheck {"""
    (for {
      a <- nonFilterableEff
      b <- filterableEff
      if a > b
    } yield a + b)
  """} must succeed

  //// No hope to pass without the '.forceFilterable' hack
  def fni = typecheck {"""
    (for {
      b <- filterableEff
      a <- nonFilterableEff.forceFilterable // hack
      if a > b
    } yield a + b)
  """} must succeed

  def nfpi = typecheck {"""
    (for {
      workaround <- nonFilterableEff *! filterableEff
      (a, b) = workaround 
      if a > b
    } yield a + b)
  """} must succeed

  def fnpi = typecheck {"""
    (for {
      workaround <- filterableEff *! nonFilterableEff
      (a, b) = workaround 
      if a > b
    } yield a + b)
  """} must succeed

  def nif = typecheck {"""
    (for {
      a <- nonFilterableEff
      if a > 0
      b <- filterableEff
    } yield a + b)
  """} must not succeed

  def fin = typecheck {"""
    (for {
      b <- filterableEff
      if b > 0
      a <- nonFilterableEff
    } yield a + b)
  """} must succeed
}
