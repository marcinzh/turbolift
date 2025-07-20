package turbolift.effects
import org.specs2.mutable._
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.effects._


class BridgeTest extends Specification:
  trait Dummy extends Signature:
    def dummy[A](a: A): A !! ThisEffect

  trait DummyEffect extends Effect[Dummy] with Dummy:
    final override def dummy[A](a: A): A !! ThisEffect = perform(_.dummy(a))

    def handler =
      new impl.Stateless[Identity, Identity, Any] with impl.Sequential with Dummy:
        override def onReturn(x: Unknown) = !!.pure(x)
        override def dummy[A](a: A) = Control.capture(k => k(a))
      .toHandler


  case object D extends DummyEffect
  case object R extends ReaderEffect[Int]

  "bridge" >> {
    (for
      a <- R.ask
      b <- R.localPut(1337):
        for
          x <- R.ask
          y <- D.dummy(x)
        yield y
      c <- R.ask
    yield s"$a $b $c")
    .handleWith(R.handler(42))
    .map(_ + " R")
    .handleWith(D.handler)
    .map(_ + " D")
    .run === "42 1337 42 R D"
  }
