package examples
import turbolift.{!!, Signature, Effect}
import turbolift.Extensions._
import turbolift.effects.{Console, IO}
import turbolift.io.Ref


case object YieldReplace extends Example:
  override def description: String = s"""
    The feature required for running this example is currently disabled,
    because it turned out to be conflicting with new ones, which were more important.
  """
  // override def description: String = """
  //   Another example adapted from "Handling Bidirectional Control Flow" paper.
  //   In addition to its original meaning,
  //   our implementation demonstrates that in Turbolift we can abstract over effect instances.
  // """

  trait YieldSignature[A] extends Signature:
    def yeld[R <: Replace[A]](R: R)(value: A): Unit !! (ThisEffect & R.type)

  trait ReplaceSignature[A] extends Signature:
    def replace(value: A): Unit !! ThisEffect

  trait Yield[A] extends Effect[YieldSignature[A]] with YieldSignature[A]:
    override def yeld[R <: Replace[A]](R: R)(value: A) = perform(_.yeld(R)(value))

  trait Replace[A] extends Effect[ReplaceSignature[A]] with ReplaceSignature[A]:
    override def replace(value: A) = perform(_.replace(value))

  def iterate[A, Y <: Yield[A], R <: Replace[A]](Y: Y, R: R)(as: List[A]): List[A] !! (Y.type & IO) =
    as match
      case Nil => Nil.pure_!!
      case a :: as =>
        for
          head <- Ref(a)
          handler =
            new R.impl.Proxy[IO] with ReplaceSignature[A]:
              override def replace(x: A) = head.put(x)
            .toHandler
          as2 <- (Y.yeld(R)(a) &&! iterate(Y, R)(as)).handleWith(handler)
          a2 <- head.get 
        yield a2 :: as2

  def replaceNegatives(as: List[Int]): List[Int] !! (Console & IO) =
    case object Y extends Yield[Int]
    case object R extends Replace[Int]
    iterate(Y, R)(as)
    .handleWith(
      new Y.impl.Proxy[Console] with YieldSignature[Int]:
        override def yeld[R <: Replace[Int]](R: R)(value: Int) =
          !!.when(value < 0)(R.replace(-value)) &&!
          Console.println(s"iterating: $value")
      .toHandler
    )

  override def apply(): Unit =
    val xs = (1 to 5).zip((1 to 5).map(_ * -10)).toList.flatMap((a, b) => List(a, b))
    (for
      ys <- replaceNegatives(xs)
      _ <- Console.println(s"input: $xs")
      _ <- Console.println(s"output: $ys")
    yield ())
    .handleWith(Console.handler)
    .unsafeRun
