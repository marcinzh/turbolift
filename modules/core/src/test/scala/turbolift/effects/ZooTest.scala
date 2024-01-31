package turbolift.effects
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{Error, Writer, State, Choice}
import turbolift.mode.ST

/** Based on "The Effect Semantics Zoo"
 *  https://github.com/lexi-lambda/eff/blob/8c4df4bf54faf22456354be18095b14825be5e85/notes/semantics-zoo.md
 */

class ZooTest extends Specification:
  "State + Error" >> {
    case object S extends State[Boolean]
    case object E extends Error[Unit]

    val hS = S.handler(false).eval
    val hE = E.handlers.first

    val prog = E.catchAll(S.put(true) &&! E.raise(()))(_ => ()) &&! S.get

    "S &&&! E" >>{ prog.handleWith(hS &&&! hE).run === Right(true) }
    "E &&&! S" >>{ prog.handleWith(hE &&&! hS).run === Right(true) }
  }


  "Choice + Error" >> {
    case object C extends Choice
    case object E extends Error[Unit]

    val hC = C.handlers.all
    val hE = E.handlers.first

    val prog1 = E.catchAll(!!.pure(true) ++! E.raise(()))(_ => false)
    val prog2 = E.catchAll(E.raise(()) ++! !!.pure(true))(_ => false)

    "E &&&! C ; 1" >>{ prog1.handleWith(hE &&&! hC).run === Vector(Right(true), Right(false)) }
    "E &&&! C ; 2" >>{ prog2.handleWith(hE &&&! hC).run === Vector(Right(false), Right(true)) }
    "C &&&! E ; 1" >>{ prog1.handleWith(hC &&&! hE).run === Right(Vector(true, false)) }
    "C &&&! E ; 2" >>{ prog2.handleWith(hC &&&! hE).run === Right(Vector(false, true)) }
  }


  "Choice + Writer" >> {
    case object C extends Choice
    case object W extends Writer[Int]

    val hC = C.handlers.all
    val hW = W.handler

    val prog = W.listen(W.tell(1) &&! (W.tell(2).as(true) ++! W.tell(3).as(false)))

    "W &&&! C" >>{ prog.handleWith(hW &&&! hC).run === Vector(((true, 3), 3), ((false, 4), 4)) }
    "C &&&! W" >>{ prog.handleWith(hC &&&! hW).run === (Vector((true, 3), (false, 4)), 7) }
  }
