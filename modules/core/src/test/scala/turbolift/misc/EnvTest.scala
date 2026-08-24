package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{ReaderEffect, StateEffect, IO}


class EnvTest extends Specification:
  "Basic ops" >> {
    "envAsk" >> {
      val n = !!.envAsk(_.tickHigh).run
      n === n
    }

    "envMod" >> {
      val prog =
        for
          a <- !!.envAsk(_.tickHigh)
          b <- !!.envMod(e => e.copy(tickHigh = (e.tickHigh * 2).toShort), {
            !!.envAsk(_.tickHigh)
          })
          c <- !!.envAsk(_.tickHigh)
        yield (a, b, c)

      val (a, b, c) = prog.run

      a * 2 === b
      a === c
    }

    "getStatus" >> {
      case object S extends StateEffect[Int]
      case object R extends ReaderEffect[Boolean]
      val prog1 = !!.isParallelizable
      val prog2 = S.put(42) &&! !!.isParallelizable
      "isParallelizable" >> {
        "no effects" >>{
          prog1.run === true
        }
        "State with local handler" >>{
          prog2.handleWith(S.handlers.local(0)).run === (false, 42)
        }
        "State with shared handler" >>{
          prog2.handleWith(S.handlers.shared(0)).runIO === (true, 42)
        }
      }
      "effects" >>{
        val prog = !!.getEffects
        val hand = S.handler(0).dropState &&&! R.handler(true)
        prog.handleWith(hand).run === Vector(IO, R, S)
      }
    }
  }

