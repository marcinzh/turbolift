package turbolift.stack_safety
import turbolift.abstraction._
import turbolift.abstraction.handlers.PartialHandler
import turbolift.std_effects._
import org.specs2._


class RepeatedlyTest extends Specification with CanStackOverflow {
  def is =
    (for {
      c <- cases
      m <- mappers
      c2 = c.mapEff(m(_))
      name = s"${c.name} effect repeatedly ${m.name} should be stack safe"
      test = br ^ name ! mustNotStackOverflow { c2.run }
    } yield test)
    .reduce(_ ^ _)

  case class Case[A, U](name: String, h: PartialHandler { type Effects = U }, eff: A !! U) {
    def mapEff[B](f: A !! U => B !! U) = copy(eff = f(eff))
    def run = h run eff
  }

  case object FxR extends Reader[Int]
  case object FxW extends Writer[Int]
  case object FxS extends State[Int]
  case object FxC extends NonDet

  val cases = List(
    Case("Reader", FxR.handler(0), FxR.ask),
    Case("Writer", FxW.handler, FxW.tell(111)),
    Case("State", FxS.handler(0), for { x <- FxS.get; _ <- FxS.put(x + 1) } yield ()),
    Case("NonDet", FxC.handler, FxC.each(List(0)))
  )

  abstract class Mapper(val name: String) {
    def apply[A, U](eff: A !! U): _ !! U
  }

  val mappers = {
    def many[A, U](eff: A !! U) = Vector.fill(TooBigForStack)(eff)
    List(
      new Mapper("chained") {
        def apply[A, U](eff: A !! U) = many(eff).reduce(_ **>! _)
      },
      new Mapper("traversed") {
        def apply[A, U](eff: A !! U) = many(eff).traverse
      }
    )
  }
}
