package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!
import turbolift.std_effects.{Reader, Writer, State}
import ReaderWriterStateHandler.Syntax._


class RWSTest extends AnyFlatSpec:
  "RWS" should "work" in {
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


    val result = (("100.5", 10.5), 0.5)

    comp
      .handleWith(FxS.handler(0.5))
      .handleWith(FxW.handler)
      .handleWith(FxR.handler(10))
      .map { case (a, (b, c)) => ((a, b), c) }
      .run
      .shouldEqual(result)

    comp
      .handleWith((FxR &! FxW &! FxS).handler(10, 0.5))
      .run
      .shouldEqual(result)
  }


  "RWS HO: localPut" should "work" in {
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

    comp
      .runWith(FxR.handler("a") <<<! (FxW.handler ***! FxS.handler("")).justState)
      .shouldEqual(result)

    comp
      .runWith((FxR &! FxW &! FxS).handler("a", "").justState)
      .shouldEqual(result)
  }


  "RWS HO: listen" should "work" in {
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
      (x, _) = workaround
      _ <- work
    yield x

    val result = (("ABA", "aba"), "B")

    comp
      .runWith(FxR.handler("a") <<<! (FxW.handler ***! FxS.handler("")))
      .shouldEqual(result)

    comp
      .runWith((FxR &! FxW &! FxS).handler("a", ""))
      .shouldEqual(result)
  }