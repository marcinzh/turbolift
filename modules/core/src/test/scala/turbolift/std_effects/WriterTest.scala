package turbolift.std_effects
import org.specs2.mutable._
import turbolift.!!
import turbolift.std_effects.{Writer, WriterK, WriterGK}
import turbolift.mode.ST


class WriterTest extends Specification:
  "Basic ops" >> {
    case object W extends Writer[Int]
    val h = W.handler

    "tell" >> {
      W.tell(1)
      .handleWith(h)
      .run === ((), 1)
    }

    "listen" >> {
      W.listen(W.tell(1))
      .handleWith(h)
      .run === (((), 1), 1)
    }

    "censor" >> {
      W.censor(_ + 1)(W.tell(1))
      .handleWith(h)
      .run === ((), 2)
    }

    "mute" >> {
      W.mute(W.tell(1))
      .handleWith(h)
      .run === ((), 0)
    }

    "pass" >> {
      W.pass(W.tell(1) **! !!.pure(_ + 2))
      .handleWith(h)
      .run === ((), 3)
    }
  }

  "Combined ops" >> {
    "tell x2" >> {
      case object W extends Writer[String]
      (W.tell("a") &&! W.tell("b"))
      .handleWith(W.handler.justState)
      .run === "ab"
    }

    "tell & listen" >> {
      case object W extends Writer[String]
      (for
        _ <- W.tell("a")
        workaround <- W.listen(W.tell("b") &&! W.tell("c"))
        ((), x) = workaround
        _ <- W.tell("d")
      yield x)
      .handleWith(W.handler) 
      .run === ("bc", "abcd")
    }

    "2 writers" >> {
      case object W1 extends Writer[Int]
      case object W2 extends Writer[String]
      (for
        _ <- W1.tell(1)
        _ <- W2.tell("a")
        _ <- W1.tell(2)
        _ <- W2.tell("b")
      yield ())
      .handleWith(W1.handler)
      .handleWith(W2.handler)
      .run === (((), 3), "ab")
    }
  }

  "Par ops" >> {
    case object W extends Writer[String]
    val h = W.handler.justState

    "tell x2 using *!" >> {
      (W.tell("a") *! W.tell("b"))
      .handleWith(h)
      .run === "ab"
    }

    "tell x2 using &!" >> {
      (W.tell("a") &! W.tell("b"))
      .handleWith(h)
      .run === "ab"
    }

    "tell x3 using &&!(&!)" >> {
      (W.tell("a") &&! (W.tell("b") &! W.tell("c")))
      .handleWith(h)
      .run === "abc"
    }

    "tell x3 using &!(&&!)" >> {
      (W.tell("a") &! (W.tell("b") &&! W.tell("c")))
      .handleWith(h)
      .run === "abc"
    }

    "tell & censor" >> {
      case object W extends Writer[String]
      (W.tell("a") &&!
      W.censor(x => s"($x)") {
        W.tell("b") *!
        W.censor(x => s"[$x]") { W.tell("c") } *!
        W.tell("d") *!
        W.censor(x => s"{$x}") { W.tell("e") } *!
        W.tell("f")
      } &&!
      W.tell("g"))
      .handleWith(W.handler.justState)
      .run === "a(b[c]d{e}f)g"
    }
  }

  "Into collections" >> {
    "WriterK" >> {
      case object W extends WriterK[Vector, Char]
      (for
        _ <- W.tell('a')
        _ <- W.tells("bc".toVector)
        _ <- W.tell('d')
      yield ())
      .handleWith(W.handler.justState)
      .run === "abcd".toVector
    }

    "WriterGK" >> {
      case object W extends WriterGK[Map, String, Vector, Int]
      (for
        _ <- W.tell("a", 1)
        _ <- W.tell("b", 10)
        _ <- W.tell("a", 2)
        _ <- W.tell("b", 20)
        _ <- W.tell("c", 100)
        _ <- W.tell("c", 200)
        _ <- W.tell("b", 30)
        _ <- W.tell("c", 300)
        _ <- W.tell("a", 3)
      yield ())
      .handleWith(W.handler.justState)
      .run === Map(
        "a" -> Vector(1, 2, 3),
        "b" -> Vector(10, 20, 30),
        "c" -> Vector(100, 200, 300),
      )
    }
  }
