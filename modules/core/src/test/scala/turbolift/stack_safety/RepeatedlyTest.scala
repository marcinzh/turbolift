package turbolift.stack_safety
import cats.implicits._
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
    val comp: Any !! Fx
    def mapEff(mapper: Mapper): This
    def run: Any
  }

  case class Case[F[_], U](name: String, h: Handler[F, U], comp: Any !! U) extends Case0 {
    type Fx = U
    def mapEff(f: Mapper) = copy(comp = f(comp))
    def run = h run comp
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
    def apply[U](comp: Any !! U): _ !! U
  }

  val mappers = {
    def many[A, U](comp: A !! U) = Vector.fill(TooBigForStack)(comp)
    List(
      new Mapper("sequentially") {
        def apply[U](comp: Any !! U) = many(comp).reduce(_ &&! _)
      },
      new Mapper("parallelly") {
        def apply[U](comp: Any !! U) = many(comp).reduce(_ &! _)
      },
      new Mapper("both sequentially and parallelly") {
        def apply[U](comp: Any !! U) = many(comp).grouped(2).map(_.reduce(_ &! _)).reduce(_ &&! _)
      },
    )
  }
}
