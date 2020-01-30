package turbolift.stack_safety
import org.specs2._


trait CanStackOverflow { this: Specification =>
  val TooBigForStack = 100000

  // def mustNotStackOverflow[A](a : => A) = a must not (throwA[java.lang.StackOverflowError])
  def mustNotStackOverflow[A](a : => A) = dump(a) must not (throwA[java.lang.StackOverflowError])

  private def dump[A](a : => A): A =
    try {
      a
    } catch {
      case e: java.lang.StackOverflowError =>
        e.printStackTrace(CanStackOverflow.writer)
        throw e
    }
}

object CanStackOverflow {
  val writer = new java.io.PrintWriter("StackOverflow.log")
}
