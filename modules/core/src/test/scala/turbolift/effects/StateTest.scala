package turbolift.effects
import org.specs2.mutable._
import org.specs2.specification.core.Fragment
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{StateEffect, IO}
import turbolift.mode.ST


class StateTest extends Specification:
  private class Picker(round: Boolean):
    def apply[T](a: => T, b: => T): T = if round then a else b
    def name = apply("local", "shared")
    def header = s"With handler = ${name}"
    def handler[S, Fx <: StateEffect[S]](fx: Fx): S => fx.ThisHandler[Identity, (_, S), IO] =
      s => apply(
        fx.handlers.local(s).tapEffK([X] => (_: (X, S)) => !!.unit.upCast[IO]),
        fx.handlers.shared(s),
      )

  private val Pickers = List(true, false).map(new Picker(_)) 

  "Basic ops" >> {
    case object S extends StateEffect[Int]
    def f(n: Int) = n * 10
    def g(n: Int) = (n.toString, n * 10)
    def ff(n: Int) = !!.pure(f(n))
    def gg(n: Int) = !!.pure(g(n))

    Fragment.foreach(Pickers) { picker =>
      val h = picker.handler(S)
      def program[A](op: A !! S.type): (A, Int) = op.handleWith(h(1)).runIO.get
      picker.header >> {
        "pure" >> {
          "get" >>{ program(S.get) === (1, 1) }
          "gets" >>{ program(S.gets(f)) === (10, 1) }
          "put" >>{ program(S.put(2)) === ((), 2) }
          "swap" >>{ program(S.swap(2)) === (1, 2) }
          "modify" >>{ program(S.modify(f)) === ((), 10) }
          "modifyGet" >>{ program(S.modifyGet(f)) === (10, 10) }
          "getModify" >>{ program(S.getModify(f)) === (1, 10) }
          "getModifyGet" >>{ program(S.getModifyGet(f)) === ((1, 10), 10) }
          "update" >>{ program(S.update(g)) === ("1", 10) }
          "updateGet" >>{ program(S.updateGet(g)) === (("1", 10), 10) }
          "getUpdate" >>{ program(S.getUpdate(g)) === (("1", 1), 10) }
          "getUpdateGet" >>{ program(S.getUpdateGet(g)) === (("1", 1, 10), 10) }
        }

        "effectful" >> {
          "getsEff" >>{ program(S.getsEff(ff)) === (10, 1) }
          "putEff" >>{ program(S.putEff(!!.pure(2))) === ((), 2) }
          "swapEff" >>{ program(S.swapEff(!!.pure(2))) === (1, 2) }
          "modifyEff" >>{ program(S.modifyEff(ff)) === ((), 10) }
          "modifyGetEff" >>{ program(S.modifyGetEff(ff)) === (10, 10) }
          "getModifyEff" >>{ program(S.getModifyEff(ff)) === (1, 10) }
          "getModifyGetEff" >>{ program(S.getModifyGetEff(ff)) === ((1, 10), 10) }
          "updateEff" >>{ program(S.updateEff(gg)) === ("1", 10) }
          "updateGetEff" >>{ program(S.updateGetEff(gg)) === (("1", 10), 10) }
          "getUpdateEff" >>{ program(S.getUpdateEff(gg)) === (("1", 1), 10) }
          "getUpdateGetEff" >>{ program(S.getUpdateGetEff(gg)) === (("1", 1, 10), 10) }
        }
      }
    }
  }


  "Combined ops" >> {
    Fragment.foreach(Pickers) { picker =>
      picker.header >> {
        "put & get" >>{
          case object S extends StateEffect[Int]
          (for
            a <- S.get
            _ <- S.put(2)
            b <- S.get
          yield (a, b))
          .handleWith(picker.handler(S)(1))
          .unsafeRun.get === ((1, 2), 2)
        }
          
        "2 states interleaved" >>{
          case object S1 extends StateEffect[Int]
          case object S2 extends StateEffect[Int]
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
