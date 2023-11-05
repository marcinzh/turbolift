package turbolift.effects
import org.specs2.mutable._
import org.specs2.specification.core.Fragment
import org.specs2.execute.Result
import turbolift.!!
import turbolift.effects.{Choice, Error}
import turbolift.mode.ST


class ChoiceTest extends Specification:
  private class Picker(round: Boolean):
    def apply[T](a: => T, b: => T): T = if round then a else b
    def name = apply("first", "all")
    def header = s"With handler = ${name}"
    def handler[Fx <: Choice](fx: Fx): fx.ThisHandler.Free[Vector] =
      apply(
        fx.handlers.first.mapK([X] => (xs: Option[X]) => xs.toVector),
        fx.handlers.all,
      )

  private val Pickers = List(true, false).map(new Picker(_)) 

  "Basic ops" >> {
    case object C extends Choice
    Fragment.foreach(Pickers) { picker =>
      val handler = picker.handler(C)
      picker.header >> {
        "choose 0" >> {
          C.choose(List())
          .handleWith(handler)
          .run === Vector[Int]()
        }

        "choose 1" >> {
          C.choose(List(1))
          .handleWith(handler)
          .run === (Vector(1))
        }

        "choose 2" >> {
          C.choose(List(1, 2))
          .handleWith(handler)
          .run === picker(
            Vector(1),
            Vector(1, 2)
          )
        }

        "fail" >> {
          val cases = List(
            ("fail &&!", C.fail &&! !!.pure(1)),
            ("fail &!",  C.fail &! !!.pure(1)),
            ("!!.fail &&!", !!.fail &&! !!.pure(1)),
            ("!!.fail &!",  !!.fail &! !!.pure(1)),
            ("&&! fail", !!.pure(1) &&! C.fail),
            ("&! fail",  !!.pure(1) &! C.fail),
            ("&&! !!.fail", !!.pure(1) &&! !!.fail),
            ("&! !!.fail",  !!.pure(1) &! !!.fail),
          )

          Fragment.foreach(cases) { case (name, prog) =>
            name >> (prog.handleWith(handler).run === Vector())
          }
        }

        "plus pure" >> {
          (!!.pure(1) ++! !!.pure(2))
          .handleWith(handler)
          .run === picker(
            Vector(1),
            Vector(1, 2)
          )
        }

        "plus fail" >> {
          (!!.pure(1) ++! !!.fail)
          .handleWith(handler)
          .run === Vector(1)
        }
      }
    }
  }


  "Combined ops" >> {
    case object C extends Choice
    Fragment.foreach(Pickers) { picker =>
      val handler = picker.handler(C)
      picker.header >> {
        "Nested choose" >> {
          (for
            n <- C.choose(1 to 2)
            c <- C.choose('a' to 'b')
          yield s"$n$c")
          .handleWith(handler)
          .run === picker(
            Vector("1a"),
            Vector("1a", "1b", "2a", "2b")
          )
        }

        "Nested choose with guard" >> {
          (for
            n <- C.choose(1 to 2)
            if n % 2 == 0
            c <- C.choose('a' to 'b')
          yield s"$n$c")
          .handleWith(handler)
          .run === picker(
            Vector("2a"),
            Vector("2a", "2b")
          )
        }

        "Nested plus" >> {
          (for
            n <- !!.pure(1) ++! !!.pure(2)
            c <- !!.pure('a') ++! !!.pure('b')
          yield s"$n$c")
          .handleWith(handler)
          .run === picker(
            Vector("1a"),
            Vector("1a", "1b", "2a", "2b")
          )
        }

        "Nested plus with guard" >> {
          (for
            n <- !!.pure(1) ++! !!.pure(2)
            if n % 2 == 0
            c <- !!.pure('a') ++! !!.pure('b')
          yield s"$n$c")
          .handleWith(handler)
          .run === picker(
            Vector("2a"),
            Vector("2a", "2b")
          )
        }

        "choose >>= tell" >> {
          case object W extends Writer[Int]
          val hW = W.handler
          val hC = picker.handler(C)

          val prog = C.choose(List(1, 2)) >>= W.tell

          "Writer &&&! Choice" >> {
            prog.handleWith(hW &&&! hC)
            .run === picker(
              Vector(((), 1)),
              (Vector(((), 1), ((), 2)))
            )
          }

          "Choice &&&! Writer" >> {
            prog.handleWith(hC &&&! hW)
            .run === picker(
              (Vector(()), 1),
              (Vector((), ()), 3)
            )
          }
        }

        "choose >>= raise" >> {
          case object E extends Error[Int]
          val hE = E.handlers.all
          val hC = picker.handler(C)

          val prog = C.choose(List(1, 2)) >>= E.raise

          "Error &&&! Choice" >> {
            prog.handleWith(hE &&&! hC)
            .run === picker(
              Vector(Left(1).withRight[Int]),
              (Vector(Left(1).withRight[Int], Left(2).withRight[Int]))
            )
          }

          "Choice &&&! Error" >> {
            prog.handleWith(hC &&&! hE)
            .run === Left(1).withRight[Int]
          }
        }

        "plus & raise" >> {
          case object E extends Error[Int]
          val hE = E.handler
          val hC = picker.handler(C)

          val prog = !!.pure(1) ++! E.raise(2)

          "Error &&&! Choice" >> {
            prog.handleWith(hE &&&! hC)
            .run === picker(
              Vector(Right(1)),
              Vector(Right(1), Left(2))
            )
          }

          "Choice &&&! Error" >> {
            prog.handleWith(hC &&&! hE)
            .run === picker(
              Right(Vector(1)),
              Left(2)
            )
          }
        }
      }
    }
  }
