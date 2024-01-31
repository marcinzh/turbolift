package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.internals.primitives.{Primitives => P}
import turbolift.mode.ST


class EnvTest extends Specification:
  "Basic ops" >> {
    "envAsk" >> {
      val n = P.envAsk(_.tickHigh).run
      n === n
    }

    "envMod" >> {
      val prog =
        for
          a <- P.envAsk(_.tickHigh)
          b <- P.envMod(e => e.copy(tickHigh = (e.tickHigh * 2).toShort), {
            P.envAsk(_.tickHigh)
          })
          c <- P.envAsk(_.tickHigh)
        yield (a, b, c)

      val (a, b, c) = prog.run

      a * 2 === b
      a === c
    }
  }

