package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{Outcome, BlockingVar}


class BlockingVarTest extends Specification:
  sequential

  "Basic ops" >> {
    def program[A](op: BlockingVar[Int] => A !! IO): (A, Int) =
      (for
        bvar <- BlockingVar(1)
        a <- op(bvar)
        nevv <- bvar.get
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
  }


  "Blocking ops" >> {
    "2 parallel updates" >>{
      (for
        bvar <- BlockingVar(1)
        aa = bvar.getModifyGetEff(x => IO.sleep(200).as(x * 10 + 2))
        bb = IO.sleep(100) &&! bvar.getModifyGet(x => x * 10 + 3)
        x <- aa *! bb
        n <- bvar.get
      yield (x, n))
      .runIO
      .===(Outcome.Success(((1, 12), (12, 123)), 123))
    }
  }

