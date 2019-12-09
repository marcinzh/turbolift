package turbolift.stack_safety
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._


trait CanStackOverflow { this: Specification =>
  val TooBigForStack = 100000

  def mustNotStackOverflow[A](a : => A) = a must not (throwA[java.lang.StackOverflowError])
}
