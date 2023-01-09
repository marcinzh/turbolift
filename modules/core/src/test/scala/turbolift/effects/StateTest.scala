package turbolift.effects
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.State
import turbolift.mode.ST


class StateTest extends Specification:
  "Basic ops" >> {
    "get" >> {
      case object S extends State[Int]
      S.get
      .handleWith(S.handler(1))
      .run === (1, 1)
    }

    "put" >> {
      case object S extends State[Int]
      S.put(2)
      .handleWith(S.handler(1))
      .run === ((), 2)
    }

    "modify" >> {
      case object S extends State[Int]
      S.modify(_ + 10)
      .handleWith(S.handler(1))
      .run === ((), 11)
    }
  }

  "Combined ops" >> {
    "put & get" >> {
      case object S extends State[Int]
      (for
        a <- S.get
        _ <- S.put(2)
        b <- S.get
      yield (a, b))
      .handleWith(S.handler(1))
      .run === ((1, 2), 2)
    }
      
    "2 states interleaved" >> {
      case object S1 extends State[Int]
      case object S2 extends State[Int]
      (for
        a <- S1.get
        b <- S2.get
        _ <- S1.modify(_ + 10 * b)
        _ <- S2.modify(_ + 10 * a)
      yield (a, b))
      .handleWith(S1.handler(1) ***! S2.handler(2))
      .run === ((1, 2), (21, 12))
    }
  }
