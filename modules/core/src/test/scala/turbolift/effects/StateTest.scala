package turbolift.effects
import org.specs2.mutable._
import org.specs2.specification.core.Fragment
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{State, IO}
import turbolift.mode.ST


class StateTest extends Specification:
  private class Picker(round: Boolean):
    def apply[T](a: => T, b: => T): T = if round then a else b
    def name = apply("local", "shared")
    def header = s"With handler = ${name}"
    def handler[S, Fx <: State[S]](fx: Fx): S => fx.ThisHandler[Identity, (_, S), IO] =
      s => apply(
        fx.handlers.local(s).tapEffK([X] => (_: (X, S)) => !!.unit.upCast[IO]),
        fx.handlers.shared(s),
      )

  private val Pickers = List(true, false).map(new Picker(_)) 

  "Basic ops" >> {
    Fragment.foreach(Pickers) { picker =>
      case object S extends State[Int]
      val h = picker.handler(S)
      picker.header >> {
        "get" >>{
          S.get
          .handleWith(h(1))
          .unsafeRun.get === (1, 1)
        }

        "put" >>{
          S.put(2)
          .handleWith(h(1))
          .unsafeRun.get === ((), 2)
        }

        "modify" >>{
          S.modify(_ + 10)
          .handleWith(h(1))
          .unsafeRun.get === ((), 11)
        }
      }
    }
  }

  "Combined ops" >> {
    Fragment.foreach(Pickers) { picker =>
      picker.header >> {
        "put & get" >>{
          case object S extends State[Int]
          (for
            a <- S.get
            _ <- S.put(2)
            b <- S.get
          yield (a, b))
          .handleWith(picker.handler(S)(1))
          .unsafeRun.get === ((1, 2), 2)
        }
          
        "2 states interleaved" >>{
          case object S1 extends State[Int]
          case object S2 extends State[Int]
          (for
            a <- S1.get
            b <- S2.get
            _ <- S1.modify(_ + 10 * b)
            _ <- S2.modify(_ + 10 * a)
          yield (a, b))
          .handleWith(picker.handler(S1)(1) ***! picker.handler(S2)(2))
          .unsafeRun.get === ((1, 2), (21, 12))
        }
      }
    }
  }
