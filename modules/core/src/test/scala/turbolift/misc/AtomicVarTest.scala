package turbolift.misc
import org.specs2.mutable._
import org.specs2.specification.core.Fragment
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.Outcome
import turbolift.io.AtomicVar


class AtomicVarTest extends Specification:
  "Basic ops" >> {
    def f(n: Int) = n * 10
    def g(n: Int) = (n.toString, n * 10)
    def ff(n: Int) = !!.pure(f(n))
    def gg(n: Int) = !!.pure(g(n))

    enum Kind { case Lockless; case Lockful }

    Fragment.foreach(List(Kind.Lockless, Kind.Lockful)) { kind =>
      kind.toString >> {
        def program[A](op: AtomicVar[Int] => A !! IO): (A, Int) =
          (for
            avar <-
              if kind == Kind.Lockless
              then AtomicVar.createLockless(1)
              else AtomicVar.createLockful(1)
            a <- op(avar)
            nevv <- avar.get
          yield (a, nevv))
          .runIO.get

        "pure" >> {
          "get" >>{ program(_.get) === (1, 1) }
          "gets" >>{ program(_.gets(f)) === (10, 1) }
          "put" >>{ program(_.put(2)) === ((), 2) }
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
          "getsEff" >>{ program(_.getsEff(ff)) === (10, 1) }
          "putEff" >>{ program(_.putEff(!!.pure(2))) === ((), 2) }
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

        "try pure" >> {
          "trySwap" >>{ program(_.trySwap(2)) === (Some(1), 2) }
          "tryModify" >>{ program(_.tryModify(f)) === (true, 10) }
          "tryModifyGet" >>{ program(_.tryModifyGet(f)) === (Some(10), 10) }
          "tryGetModify" >>{ program(_.tryGetModify(f)) === (Some(1), 10) }
          "tryGetModifyGet" >>{ program(_.tryGetModifyGet(f)) === (Some((1, 10)), 10) }
          "tryUpdate" >>{ program(_.tryUpdate(g)) === (Some("1"), 10) }
          "tryUpdateGet" >>{ program(_.tryUpdateGet(g)) === (Some(("1", 10)), 10) }
          "tryGetUpdate" >>{ program(_.tryGetUpdate(g)) === (Some(("1", 1)), 10) }
          "tryGetUpdateGet" >>{ program(_.tryGetUpdateGet(g)) === (Some(("1", 1, 10)), 10) }
        }

        "try effectful" >> {
          "trySwapEff" >>{ program(_.trySwapEff(!!.pure(2))) === (Some(1), 2) }
          "tryModifyEff" >>{ program(_.tryModifyEff(ff)) === (true, 10) }
          "tryModifyGetEff" >>{ program(_.tryModifyGetEff(ff)) === (Some(10), 10) }
          "tryGetModifyEff" >>{ program(_.tryGetModifyEff(ff)) === (Some(1), 10) }
          "tryGetModifyGetEff" >>{ program(_.tryGetModifyGetEff(ff)) === (Some((1, 10)), 10) }
          "tryUpdateEff" >>{ program(_.tryUpdateEff(gg)) === (Some("1"), 10) }
          "tryUpdateGetEff" >>{ program(_.tryUpdateGetEff(gg)) === (Some(("1", 10)), 10) }
          "tryGetUpdateEff" >>{ program(_.tryGetUpdateEff(gg)) === (Some(("1", 1)), 10) }
          "tryGetUpdateGetEff" >>{ program(_.tryGetUpdateGetEff(gg)) === (Some(("1", 1, 10)), 10) }
        }
      }
    }
  }

  "Blocking ops" >> {
    "2 parallel updates" >>{
      (for
        bvar <- AtomicVar.createLockful(1)
        aa = bvar.getModifyGetEff(x => IO.sleep(200).as(x * 10 + 2))
        bb = IO.sleep(100) &&! bvar.getModifyGet(x => x * 10 + 3)
        x <- aa *! bb
        n <- bvar.get
      yield (x, n))
      .runIO
      .===(Outcome.Success(((1, 12), (12, 123)), 123))
    }
  }
