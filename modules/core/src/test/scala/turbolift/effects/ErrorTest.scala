package turbolift.effects
import org.specs2.mutable._
import org.specs2.specification.core.Fragment
import org.specs2.execute.Result
import turbolift.{!!, Handler}
import turbolift.typeclass.Accum
import turbolift.effects.{Error, State}
import turbolift.mode.ST


class ErrorTest extends Specification with CanLaunchTheMissiles:
  private class Picker(round: Boolean):
    def apply[T](a: => T, b: => T): T = if round then a else b
    def name = apply("first", "all")
    def handler[T, Fx <: Error[T]](fx: Fx)(using Accum[T, T]): fx.ThisHandler.Free[Either[T, *]] = apply(fx.handlers.first, fx.handlers.all)

  private val Pickers = List(true, false).map(new Picker(_)) 

  "Basic ops" >> {
    "raise" >> {
      case object E extends Error[Int]
      val missile = Missile()
      (E.raise(1) &&! missile.launch_!)
      .handleWith(E.handler)
      .run === Left(1)
      
      missile.mustNotHaveLaunched
    }

    "catchAll" >> {
      case object E extends Error[Int]
      E.catchAll(E.raise(1))(_ => !!.pure(2))
      .handleWith(E.handler)
      .run === Right(2)
    }
  }

  "Combined ops" >> {
    "raise & put" >> {
      case object E extends Error[Int]
      case object S extends State[Int]
      val prog =
        for
          _ <- S.put(1)
          _ <- E.raise(42)
          _ <- S.put(2)
        yield ()

      Fragment.foreach(Pickers) { picker =>
        val hS = S.handler(0)
        val hE = picker.handler(E)
        s"With handler = ${picker.name}" >> {
          "State &&&! Error" >> {
            prog.handleWith(hS &&&! hE).run === Left(42)
          }

          "Error &&&! State" >> {
            prog.handleWith(hE &&&! hS).run === (Left(42), 1)
          }
        }
      }
    }

    "catchAll & put before raise" >> {
      case object E extends Error[Int]
      case object S extends State[Int]
      val prog =
        E.catchAll {
          for
            _ <- S.put(1)
            _ <- E.raise(42)
            _ <- S.put(2)
          yield true
        } (_ => !!.pure(false))

      Fragment.foreach(Pickers) { picker =>
        val hS = S.handler(0)
        val hE = picker.handler(E)
        s"With handler = ${picker.name}" >> {
          "State &&&! Error" >> {
            prog.handleWith(hS &&&! hE).run === Right((false, 1))
          }

          "Error &&&! State" >> {
            prog.handleWith(hE &&&! hS).run === (Right(false), 1)
          }
        }
      }
    }

    "catchAll & put after raise" >> {
      case object E extends Error[Unit]
      case object S extends State[Int]
      val prog = E.catchAll(E.raise(()))(_ => S.put(10))

      Fragment.foreach(Pickers) { picker =>
        val hS = S.handler(0)
        val hE = picker.handler(E)
        s"With handler = ${picker.name}" >> {
          "State &&&! Error" >> {
            prog.handleWith(hS &&&! hE).run === Right(((), 10))
          }

          "Error &&&! State" >> {
            prog.handleWith(hE &&&! hS).run === (Right(()), 10)
          }
        }
      }
    }

    "catchAll & localPut" >> {
      case object R extends Reader[Int]
      case object E extends Error[Unit]
      val prog = E.catchAll(R.localPut(2)(E.raise(())))(_ => R.ask)

      Fragment.foreach(Pickers) { picker =>
        val hR = R.handler(1)
        val hE = picker.handler(E)

        "Reader &&&! Error" >> {
          prog.handleWith(hR &&&! hE).run === Right(1)
        }

        "Error &&&! Reader" >> {
          prog.handleWith(hE &&&! hR).run === Right(1)
        }
      }
    }
  }


  "Par ops" >> {
    Fragment.foreach(Pickers) { picker =>
      s"With handler = ${picker.name}" >> {
        "raise *!" >> {
          case object E extends Error[Int]
          (E.raise(1) *! E.raise(2))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(3).withRight[Int]
          )
        }

        "raise *! *!" >> {
          case object E extends Error[Int]
          (E.raise(1) *! E.raise(2) *! E.raise(10))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(13).withRight[Int]
          )
        }

        "raise *! &&!" >> {
          case object E extends Error[Int]
          (E.raise(1) *! E.raise(2) &&! E.raise(10))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(3).withRight[Int]
          )
        }

        "sequentially raise &! raise" >> {
          case object E extends Error[Int]
          !!.sequentially(E.raise(1) &! E.raise(2))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(1).withRight[Int]
          )
        }

        "parallelly raise &! raise" >> {
          case object E extends Error[Int]
          !!.parallelly(E.raise(1) &! E.raise(2))
          .handleWith(picker.handler(E))
          .run === picker(
            Left(1).withRight[Int],
            Left(3).withRight[Int]
          )
        }

        "raise & tell" >> {
          case object E extends Error[String]
          case object W extends Writer[String]
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

          "Writer &&&! Error" >> {
            prog.handleWith(hW &&&! hE).run === Left(err)
          }

          "Error &&&! Writer" >> {
            prog.handleWith(hE &&&! hW).run === (Left(err), acc)
          }
        }

        "catchAll & tell" >> {
          case object E extends Error[String]
          case object W extends Writer[String]
          val prog = 
            E.catchAll {
              for
                _ <- W.tell("a")
                _ <- W.tell("b") *! E.raise("x") *! W.tell("c") *! E.raise("y")
                _ <- W.tell("d")
                _ <- E.raise("z")
              yield "?"
            } (str => !!.pure(str.toUpperCase))

          val hE = picker.handler(E)
          val hW = W.handler
          val err = picker("X", "XY")

          "Writer &&&! Error" >> {
            val acc = picker("ab", "a")
            prog.handleWith(hW &&&! hE).run === Right((err, acc))
          }

          "Error &&&! Writer" >> {
            val acc = picker("ab", "abc")
            prog.handleWith(hE &&&! hW).run === (Right(err), acc)
          }
        }
      }
    }
  }
