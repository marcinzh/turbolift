package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{Outcome, AtomicVar}


class AtomicVarTest extends Specification:
  "Basic ops" >> {
    def program[A](op: AtomicVar[Int] => A !! IO): (A, Int) =
      (for
        avar <- AtomicVar(1)
        a <- op(avar)
        nevv <- avar.get
      yield (a, nevv))
      .runIO.get

    def f(n: Int) = n * 10
    def g(n: Int) = (n.toString, n * 10)
    def ff(n: Int) = !!.pure(f(n))
    def gg(n: Int) = !!.pure(g(n))

    "pure" >> {
      "swap" >>{ program(_.swap(2)) === (1, 2) }
      "modify" >>{ program(_.modify(f)) === ((), 10) }
      "modifyGet" >>{ program(_.modifyGet(f)) === (10, 10) }
      "getModify" >>{ program(_.getModify(f)) === (1, 10) }
      "getModifyGet" >>{ program(_.getModifyGet(f)) === ((1, 10), 10) }
      "update" >>{ program(_.update(g)) === ("1", 10) }
      "updateGet" >>{ program(_.updateGet(g)) === (("1", 10), 10) }
      "getUpdate" >>{ program(_.getUpdate(g)) === (("1", 1), 10) }
      "getUpdateGet" >>{ program(_.getUpdateGet(g)) === (("1", 1, 10), 10) }
    }

    "effectful" >> {
      "swapEff" >>{ program(_.swapEff(!!.pure(2))) === (1, 2) }
      "modifyEff" >>{ program(_.modifyEff(ff)) === ((), 10) }
      "modifyGetEff" >>{ program(_.modifyGetEff(ff)) === (10, 10) }
      "getModifyEff" >>{ program(_.getModifyEff(ff)) === (1, 10) }
      "getModifyGetEff" >>{ program(_.getModifyGetEff(ff)) === ((1, 10), 10) }
      "updateEff" >>{ program(_.updateEff(gg)) === ("1", 10) }
      "updateGetEff" >>{ program(_.updateGetEff(gg)) === (("1", 10), 10) }
      "getUpdateEff" >>{ program(_.getUpdateEff(gg)) === (("1", 1), 10) }
      "getUpdateGetEff" >>{ program(_.getUpdateGetEff(gg)) === (("1", 1, 10), 10) }
    }

    "try" >> {
      "trySwap" >>{ program(_.trySwap(2)) === ((1, true), 2) }
      "tryModify" >>{ program(_.tryModify(f)) === (true, 10) }
      "tryModifyGet" >>{ program(_.tryModifyGet(f)) === ((10, true), 10) }
      "tryGetModify" >>{ program(_.tryGetModify(f)) === ((1, true), 10) }
      "tryGetModifyGet" >>{ program(_.tryGetModifyGet(f)) === ((1, 10, true), 10) }
      "tryUpdate" >>{ program(_.tryUpdate(g)) === (("1", true), 10) }
      "tryUpdateGet" >>{ program(_.tryUpdateGet(g)) === (("1", 10, true), 10) }
      "tryGetUpdate" >>{ program(_.tryGetUpdate(g)) === (("1", 1, true), 10) }
      "tryGetUpdateGet" >>{ program(_.tryGetUpdateGet(g)) === (("1", 1, 10, true), 10) }
    }
  }
