package turbolift.effects
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{Error, Writer, State, Choice}
import turbolift.mode.ST


class ZooTest extends Specification:
  "State + Error" >> {
    case object S extends State[Boolean]
    case object E extends Error[Unit]

    val hS = S.handler(false).eval
    val hE = E.handlers.first

    val prog = E.catchAll(S.put(true) &&! E.raise(()))(_ => !!.unit) &&! S.get

    val se = prog.handleWith(hS).handleWith(hE)
    val es = prog.handleWith(hE).handleWith(hS)

    "S &&&! E" >> { se.run === Right(true) }
    "E &&&! S" >> { es.run === Right(true) }
  }


  "Choice + Error" >> {
    case object C extends Choice
    case object E extends Error[Unit]

    val hC = C.handlers.all
    val hE = E.handlers.first
    val hEC = hE &&&! hC
    val hCE = hC &&&! hE

    val prog1 = E.catchAll(!!.pure(true) ++! E.raise(()))(_ => !!.pure(false))
    val prog2 = E.catchAll(E.raise(()) ++! !!.pure(true))(_ => !!.pure(false))

    "E &&&! C ; 1" >> { prog1.handleWith(hEC).run === Vector(Right(true), Right(false)) }
    "E &&&! C ; 2" >> { prog2.handleWith(hEC).run === Vector(Right(false), Right(true)) }
    "C &&&! E ; 1" >> { prog1.handleWith(hCE).run === Right(Vector(true, false)) }
    "C &&&! E ; 2" >> { prog2.handleWith(hCE).run === Right(Vector(false, true)) }
  }


  "Choice + Writer" >> {
    case object C extends Choice
    case object W extends Writer[Int]

    val hC = C.handlers.all
    val hW = W.handler
    val hWC = hW &&&! hC
    val hCW = hC &&&! hW

    val prog = W.listen(W.tell(1) &&! (W.tell(2).as(true) ++! W.tell(3).as(false)))

    "W &&&! C" >> { prog.handleWith(hWC).run === Vector(((true, 3), 3), ((false, 4), 4)) }
    "C &&&! W" >> { prog.handleWith(hCW).run === (Vector((true, 3), (false, 4)), 7) }
  }
