package turbolift.effects
import org.specs2.mutable._
import org.specs2.specification.core.Fragment
import org.specs2.execute.Result
import turbolift.{!!, Handler}
import turbolift.Extensions._
import turbolift.effects.{ErrorEffect, StateEffect}
import turbolift.typeclass.Accum
import turbolift.mode.ST


class ErrorTest extends Specification with CanLaunchTheMissiles:
  private class Picker(round: Boolean):
    def apply[T](a: => T, b: => T): T = if round then a else b
    def name = apply("first", "all")
    def header = s"With handler = ${name}"
    def handler[T, Fx <: ErrorEffect[T]](fx: Fx)(using Accum[T, T]): fx.ThisHandler[Identity, Either[T, _], Any] =
      apply(fx.handlers.first, fx.handlers.all)

  private val Pickers = List(true, false).map(new Picker(_))

  "Basic ops" >> {
    "raise" >>{
      case object E extends ErrorEffect[Int]
      val missile = Missile()
      (E.raise(1) &&! missile.launch_!)
      .handleWith(E.handler)
      .run === Left(1)
      
      missile.mustNotHaveLaunched
    }

    "catchAll" >>{
      case object E extends ErrorEffect[Int]
      E.catchAll(E.raise(1))(_ => 2)
      .handleWith(E.handler)
      .run === Right(2)
    }
  }


  "Combined ops" >> {
    "raise & put" >> {
      case object E extends ErrorEffect[Int]
      case object S extends StateEffect[Int]
      val prog =
        for
          _ <- S.put(1)
          _ <- E.raise(42)
          _ <- S.put(2)
        yield ()

      Fragment.foreach(Pickers) { picker =>
        val hS = S.handler(0)
        val hE = picker.handler(E)
        picker.header >> {
          "State &&&! Error" >>{
            prog.handleWith(hS &&&! hE).run === Left(42)
          }

          "Error &&&! State" >>{
            prog.handleWith(hE &&&! hS).run === (Left(42), 1)
          }
        }
      }
    }

    "catchAll & put before raise" >> {
      case object E extends ErrorEffect[Int]
      case object S extends StateEffect[Int]
      val prog =
        E.catchAll {
          for
            _ <- S.put(1)
            _ <- E.raise(42)
            _ <- S.put(2)
          yield true
        } (_ => false)

      Fragment.foreach(Pickers) { picker =>
        val hS = S.handler(0)
        val hE = picker.handler(E)
        picker.header >> {
          "State &&&! Error" >>{
            prog.handleWith(hS &&&! hE).run === Right((false, 1))
          }

          "Error &&&! State" >>{
            prog.handleWith(hE &&&! hS).run === (Right(false), 1)
          }
        }
      }
    }

    "catchAll & put after raise" >> {
      case object E extends ErrorEffect[Unit]
      case object S extends StateEffect[Int]
      val prog = E.catchAllEff(E.raise(()))(_ => S.put(10))

      Fragment.foreach(Pickers) { picker =>
        val hS = S.handler(0)
        val hE = picker.handler(E)
        picker.header >> {
          "State &&&! Error" >>{
            prog.handleWith(hS &&&! hE).run === Right(((), 10))
          }

          "Error &&&! State" >>{
            prog.handleWith(hE &&&! hS).run === (Right(()), 10)
          }
        }
      }
    }

    "catchAll & localPut" >> {
      case object R extends ReaderEffect[Int]
      case object E extends ErrorEffect[Unit]
      val prog =
        E.catchAllEff(R.localPut(2)(E.raise(())))(_ => R.ask)

      Fragment.foreach(Pickers) { picker =>
        val hR = R.handler(1)
        val hE = picker.handler(E)

        "Reader &&&! Error" >>{
          prog.handleWith(hR &&&! hE).run === Right(1)
        }

        "Error &&&! Reader" >>{
          prog.handleWith(hE &&&! hR).run === Right(1)
        }
      }
    }
  }


  "Par ops" >>  {
    Fragment.foreach(Pickers) { picker =>
      picker.header >> {
        "raise *!" >>{
          case object E extends ErrorEffect[Int]
          (E.raise(1) *! E.raise(2))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(3).withRight[Int]
          )
        }

        "raise *! *!" >>{
          case object E extends ErrorEffect[Int]
          (E.raise(1) *! E.raise(2) *! E.raise(10))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(13).withRight[Int]
          )
        }

        "raise *! &&!" >>{
          case object E extends ErrorEffect[Int]
          (E.raise(1) *! E.raise(2) &&! E.raise(10))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(3).withRight[Int]
          )
        }

        "sequentially raise &! raise" >>{
          case object E extends ErrorEffect[Int]
          !!.sequentially(E.raise(1) &! E.raise(2))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(1).withRight[Int]
          )
        }

        "parallelly raise &! raise" >>{
          case object E extends ErrorEffect[Int]
          !!.parallelly(E.raise(1) &! E.raise(2))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(3).withRight[Int]
          )
        }

        "raise & tell" >> {
          case object E extends ErrorEffect[String]
          case object W extends WriterEffect[String]
          val prog = 
            for
              _ <- W.tell("a")
              _ <- W.tell("b") *! E.raise("x") *! W.tell("c") *! E.raise("y")
              _ <- W.tell("d")
              _ <- E.raise("z")
            yield ()

          val hE = picker.handler(E)
          val hW = W.handler
          val err = picker("x", "xy")
          val acc = picker("ab", "abc")

          "Writer &&&! Error" >>{
            prog.handleWith(hW &&&! hE).run === Left(err)
          }

          "Error &&&! Writer" >>{
            prog.handleWith(hE &&&! hW).run === (Left(err), acc)
          }
        }

        "catchAll & tell (simpler)" >> {
          case object E extends ErrorEffect[String]
          case object W extends WriterEffect[Int]
          val prog =
            E.catchAll {
              for
                _ <- W.tell(1) *! E.raise("x") *! W.tell(10) 
              yield "?"
            } (str => str.toUpperCase)

          val hE = picker.handler(E)
          val hW = W.handler

          "Writer &&&! Error" >>{
            val acc = picker(1, 0)
            prog.handleWith(hW &&&! hE).run === Right(("X", acc))
          }

          "Error &&&! Writer" >>{
            val acc = picker(1, 11)
            prog.handleWith(hE &&&! hW).run === (Right("X"), acc)
          }
        }

        "catchAll & tell" >> {
          case object E extends ErrorEffect[String]
          case object W extends WriterEffect[String]
          val prog = 
            E.catchAll {
              for
                _ <- W.tell("a")
                _ <- W.tell("b") *! E.raise("x") *! W.tell("c") *! E.raise("y")
                _ <- W.tell("d")
                _ <- E.raise("z")
              yield "?"
            } (str => str.toUpperCase)

          val hE = picker.handler(E)
          val hW = W.handler
          val err = picker("X", "XY")

          "Writer &&&! Error" >>{
            val acc = picker("ab", "a")
            prog.handleWith(hW &&&! hE).run === Right((err, acc))
          }

          "Error &&&! Writer" >>{
            val acc = picker("ab", "abc")
            prog.handleWith(hE &&&! hW).run === (Right(err), acc)
          }
        }
      }
    }
  }
