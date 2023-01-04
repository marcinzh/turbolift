package turbolift.std_effects
import org.specs2.mutable._
import turbolift.!!
import turbolift.std_effects.{Reader, WriterK}
import turbolift.mode.ST


class ReaderTest extends Specification:
  "Basic ops" >> {
    "ask" >> {
      case object R extends Reader[Int]
      R.ask
      .handleWith(R.handler(1))
      .run === 1
    }

    "asks" >> {
      case object R extends Reader[(Int, Int)]
      R.asks(_._1)
      .handleWith(R.handler((1, 2)))
      .run === 1
    }

    "localPut" >> {
      case object R extends Reader[Int]
      R.localPut(2) { R.ask }
      .handleWith(R.handler(1))
      .run === 2
    }

    "localModify" >> {
      case object R extends Reader[Int]
      R.localModify(_ + 2) { R.ask }
      .handleWith(R.handler(1))
      .run === 3
    }
  }


  "Combined ops" >> {
    "ask **! localPut" >> {
      case object R extends Reader[Int]
      (R.ask **! R.localPut(2) { R.ask })
      .handleWith(R.handler(1))
      .run === (1, 2)
    }

    "localPut **! ask" >> {
      case object R extends Reader[Int]
      (R.localPut(2) { R.ask } **! R.ask)
      .handleWith(R.handler(1))
      .run === (2, 1)
    }

    "nested localModify x1" >> {
      case object R extends Reader[Int]
      (
        R.ask **!
        R.localModify(_ + 10) {
          R.ask
        } **!
        R.ask
      )
      .handleWith(R.handler(1))
      .run === ((1, 11), 1)
    }


    "nested localModify x2" >> {
      case object R extends Reader[Int]
      (
        R.ask **!
        R.localModify(_ + 1) {
          R.ask **!
          R.localModify(_ + 10) {
            R.ask
          } **!
          R.ask
        } **!
        R.ask
      )
      .handleWith(R.handler(1))
      .run === ((1, ((2, 12), 2)), 1)
    }

    "nested localModify x3" >> {
      case object R extends Reader[Int]
      (
        R.ask **!
        R.localModify(_ + 1) {
          R.ask **!
          R.localModify(_ + 10) {
            R.ask **!
            R.localModify(_ + 100) {
              R.ask
            } **!
            R.ask
          } **!
          R.ask
        } **!
        R.ask
      )
      .handleWith(R.handler(1))
      .run === ((1,((2,((12,112),12)),2)),1)
    }
  }


  "Par ops" >> {
    case object R extends Reader[Int]
    val h = R.handler(1)

    "ask *! localPut" >> {
      (R.ask *! R.localPut(2)(R.ask))
      .handleWith(h)
      .run === ((1, 2))
    }

    "localPut *! ask" >> {
      (R.localPut(2)(R.ask) *! R.ask)
      .handleWith(h)
      .run === ((2, 1))
    }

    "nested localModify" >> {
      R.localModify(_ + 1) {
        R.ask *!
        R.localModify(_ + 10) { R.ask } *!
        R.ask *!
        R.localModify(_ + 100) { R.ask } *!
        R.ask
      }
      .map { case ((((a, b), c), d), e) => (a, b, c, d, e) }
      .handleWith(h)
      .run === ((2, 12, 2, 102, 2))
    }
  }
