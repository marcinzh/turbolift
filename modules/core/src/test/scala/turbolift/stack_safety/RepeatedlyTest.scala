package turbolift.stack_safety
import turbolift.abstraction.{!!, Handler}
import turbolift.std_effects.{Reader, Writer, State, Choice}
import org.specs2._


class RepeatedlyTest extends Specification with CanStackOverflow {
  def is =
    (for {
      c <- cases
      m <- mappers
      c2 = c.mapEff(m)
      name = s"${c.name} effect composed ${m.name}, should be stack safe"
      test = br ^ name ! mustNotStackOverflow { c2.run }
    } yield test)
    .reduce(_ ^ _)

  trait Case0 { outer =>
    type Fx
    type This = Case0 { type Fx = outer.Fx }
    val name: String
    val eff: Any !! Fx
    def mapEff(mapper: Mapper): This
    def run: Any
  }

  case class Case[U](name: String, h: Handler { type Effects = U }, eff: Any !! U) extends Case0 {
    type Fx = U
    def mapEff(f: Mapper) = copy(eff = f(eff))
    def run = h run eff
  }

  case object FxR extends Reader[Int]
  case object FxW extends Writer[Int]
  case object FxS extends State[Int]
  case object FxC extends Choice

  val cases = List[Case0](
    Case("Reader", FxR.handler(0), FxR.ask),
    Case("Writer", FxW.handler, FxW.tell(111)),
    Case("State", FxS.handler(0), FxS.mod(_ + 1)),
    Case("Choice", FxC.handler, FxC.each(List(0)))
  )

  abstract class Mapper(val name: String) {
    def apply[U](eff: Any !! U): _ !! U
  }

  val mappers = {
    def many[A, U](eff: A !! U) = Vector.fill(TooBigForStack)(eff)
    List(
      new Mapper("sequentially") {
        def apply[U](eff: Any !! U) = many(eff).reduce(_ &&! _)
      },
      new Mapper("parallelly") {
        def apply[U](eff: Any !! U) = many(eff).reduce(_ &! _)
      },
      new Mapper("both sequentially and parallelly") {
        def apply[U](eff: Any !! U) = many(eff).grouped(2).map(_.reduce(_ &! _)).reduce(_ &&! _)
      },
    )
  }
}
