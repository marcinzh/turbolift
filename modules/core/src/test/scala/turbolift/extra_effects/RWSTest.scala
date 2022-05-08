package turbolift.extra_effects
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!
import turbolift.std_effects.{Reader, Writer, State}
import turbolift.extra_effects.ReaderWriterState.Syntax._


class RWSTest extends AnyFunSpec:
  describe("Basic ops") {
    case object FxR extends Reader[Int]
    case object FxW extends Writer[String]
    case object FxS extends State[Double]

    val comp = for
      r <- FxR.ask
      _ <- FxW.tell(r.toString)
      s <- FxS.get
      _ <- FxW.tell(s.toString)
      _ <- FxS.put(s + r)
    yield s

    val result = (0.5, ("100.5", 10.5))

    it("R+W+S") {
      comp
        .handleWith(FxW.handler ***! FxS.handler(0.5))
        .handleWith(FxR.handler(10))
        .run
        .shouldEqual(result)
    }

    it("RWS") {
      comp
        .handleWith((FxR &! FxW &! FxS).handler(10, 0.5))
        .run
        .shouldEqual(result)
    }
  }

  describe("RWS HO-ops") {
    describe("localPut") {
      case object FxR extends Reader[String]
      case object FxW extends Writer[String]
      case object FxS extends State[String]

      val work = for
        x <- FxR.ask
        _ <- FxS.modify(_ ++ x)
        _ <- FxW.tell(x.toUpperCase)
      yield ()

      val comp = for
        _ <- work
        _ <- FxR.localPut("b")(work)
        _ <- work
      yield ()

      val result = ("ABA", "aba")

      it("R+W+S") {
        comp
          .runWith(FxR.handler("a") &&&! (FxW.handler ***! FxS.handler("")).justState)
          .shouldEqual(result)
      }

      it("RWS") {
        comp
          .runWith((FxR &! FxW &! FxS).handler("a", "").justState)
          .shouldEqual(result)
      }
    }


    describe("listen") {
      case object FxR extends Reader[String]
      case object FxW extends Writer[String]
      case object FxS extends State[String]

      val work = for
        x <- FxR.ask
        _ <- FxS.modify(_ ++ x)
        _ <- FxW.tell(x.toUpperCase)
      yield ()

      val comp = for
        _ <- work
        workaround <- FxW.listen(FxR.localPut("b")(work))
        ((), x) = workaround
        _ <- work
      yield x

      val result = ("B", ("ABA", "aba"))

      it("R+W+S") {
        comp
          .runWith(FxR.handler("a") &&&! (FxW.handler ***! FxS.handler("")))
          .shouldEqual(result)
      }

      it("RWS") {
        comp
          .runWith((FxR &! FxW &! FxS).handler("a", ""))
          .shouldEqual(result)
      }
    }
  }
  