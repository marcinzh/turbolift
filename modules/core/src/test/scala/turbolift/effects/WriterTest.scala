package turbolift.effects
import org.specs2.mutable._
import org.specs2.specification.core.Fragment
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{Writer, WriterK, WriterGK, IO}
import turbolift.typeclass.AccumZero
import turbolift.mode.ST


class WriterTest extends Specification:
  private class Picker(round: Boolean):
    def apply[T](a: => T, b: => T): T = if round then a else b
    def name = apply("local", "shared")
    def header = s"With handler = ${name}"
    def handler[W, W1, Fx <: WriterEffect[W, W1]](fx: Fx)(using AccumZero[W, W1]): fx.ThisHandler[Identity, (_, W), IO] =
      apply(
        fx.handlers.local.tapEffK([X] => (_: (X, W)) => !!.unit.upCast[IO]),
        fx.handlers.shared,
      )

  private val Pickers = List(true, false).map(new Picker(_)) 

  "Basic ops" >> {
    case object W extends Writer[Int]
    Fragment.foreach(Pickers) { picker =>
      val h = picker.handler(W)
      picker.header >> {
        "tell" >>{
          W.tell(1)
          .handleWith(h)
          .unsafeRun.get === ((), 1)
        }

        "listen" >>{
          W.listen(W.tell(1))
          .handleWith(h)
          .unsafeRun.get === (((), 1), 1)
        }

        "censor" >>{
          W.censor(_ + 1)(W.tell(1))
          .handleWith(h)
          .unsafeRun.get === ((), 2)
        }

        "mute" >>{
          W.mute(W.tell(1))
          .handleWith(h)
          .unsafeRun.get === ((), 0)
        }

        "pass" >>{
          W.pass(W.tell(1) **! !!.pure(_ + 2))
          .handleWith(h)
          .unsafeRun.get === ((), 3)
        }
      }
    }
  }

  "Combined ops" >> {
    Fragment.foreach(Pickers) { picker =>
      picker.header >> {
        "tell x2" >>{
          case object W extends Writer[String]
          (W.tell("a") &&! W.tell("b"))
          .handleWith(picker.handler(W).justState)
          .unsafeRun.get === "ab"
        }

        "tell & listen" >>{
          case object W extends Writer[String]
          (for
            _ <- W.tell("a")
            workaround <- W.listen(W.tell("b") &&! W.tell("c"))
            ((), x) = workaround
            _ <- W.tell("d")
          yield x)
          .handleWith(picker.handler(W))
          .unsafeRun.get === ("bc", "abcd")
        }

        "2 writers" >>{
          case object W1 extends Writer[Int]
          case object W2 extends Writer[String]
          (for
            _ <- W1.tell(1)
            _ <- W2.tell("a")
            _ <- W1.tell(2)
            _ <- W2.tell("b")
          yield ())
          .handleWith(picker.handler(W1))
          .handleWith(picker.handler(W2))
          .unsafeRun.get === (((), 3), "ab")
        }
      }
    }
  }

  "Par ops" >> {
    case object W extends Writer[String]
    Fragment.foreach(Pickers) { picker =>
      val h = picker.handler(W).justState
      picker.header >> {
        "tell x2 using *!" >>{
          (W.tell("a") *! W.tell("b"))
          .handleWith(h)
          .unsafeRun.get === "ab"
        }

        "tell x2 using &!" >>{
          (W.tell("a") &! W.tell("b"))
          .handleWith(h)
          .unsafeRun.get === "ab"
        }

        "tell x3 using &&!(&!)" >>{
          (W.tell("a") &&! (W.tell("b") &! W.tell("c")))
          .handleWith(h)
          .unsafeRun.get === "abc"
        }

        "tell x3 using &!(&&!)" >>{
          (W.tell("a") &! (W.tell("b") &&! W.tell("c")))
          .handleWith(h)
          .unsafeRun.get === "abc"
        }

        "tell & censor" >>{
          (W.tell("a") &&!
          W.censor(x => s"($x)") {
            W.tell("b") &! (
            W.censor(x => s"[$x]") { W.tell("c") } &! (
            W.tell("d") &! (
            W.censor(x => s"{$x}") { W.tell("e") } &! (
            W.tell("f")
            )))) 
          } &&!
          W.tell("g"))
          .handleWith(h)
          .unsafeRun.get === "a(b[c]d{e}f)g"
        }
      }
    }
  }

  "Into collections" >> {
    Fragment.foreach(Pickers) { picker =>
      picker.header >> {
        "WriterK" >>{
          case object W extends WriterK[Vector, Char]
          (for
            _ <- W.tell('a')
            _ <- W.tells("bc".toVector)
            _ <- W.tell('d')
          yield ())
          .handleWith(picker.handler(W).justState)
          .unsafeRun.get === "abcd".toVector
        }

        "WriterGK" >>{
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
          .handleWith(picker.handler(W).justState)
          .unsafeRun.get === Map(
            "a" -> Vector(1, 2, 3),
            "b" -> Vector(10, 20, 30),
            "c" -> Vector(100, 200, 300),
          )
        }
      }
    }
  }
