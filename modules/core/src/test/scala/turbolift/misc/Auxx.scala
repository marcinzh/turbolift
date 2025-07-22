package turbolift.misc
import java.util.concurrent.CountDownLatch
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.Outcome
import turbolift.io.AtomicVar


object Auxx:
  final class Gate private (cdl: CountDownLatch):
    def unsafeOpen() = cdl.countDown()
    def unsafeEnter() = cdl.await()
    def open = IO(unsafeOpen())
    def enter = IO(unsafeEnter())

  object Gate:
    def apply(n: Int): Gate !! IO = IO(new Gate(new CountDownLatch(n)))

  extension (thiz: AtomicVar[Int])
    def event(d: Int) = thiz.modify(n => n * 10 + d)

  extension (thiz: AtomicVar[Long])
    @annotation.targetName("event_Long")
    def event(d: Int) = thiz.modify(n => n * 10 + d)
