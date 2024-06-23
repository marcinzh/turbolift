package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.mode.ST


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
  }

