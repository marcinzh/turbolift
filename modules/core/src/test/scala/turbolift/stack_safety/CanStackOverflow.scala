package turbolift.stack_safety
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


trait CanStackOverflow:
  this: AnyFlatSpec =>

  export CanStackOverflow.TooBigForStack
  
  def mustNotStackOverflow[A](a: => A) =
    try
      a
    catch
      case e: java.lang.StackOverflowError =>
        e.printStackTrace(CanStackOverflow.writer)
        fail("Stack Overflow")


object CanStackOverflow:
  val TooBigForStack = 100000
  val writer = new java.io.PrintWriter("StackOverflow.log")
