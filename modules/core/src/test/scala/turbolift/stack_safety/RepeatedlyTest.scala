package turbolift.stack_safety
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.{!!, Handler}
import turbolift.std_effects.{Reader, Writer, State, Choice}


class RepeatedlyTest extends AnyFunSpec with CanStackOverflow:
  import RepeatedlyTest_Stuff.{cases, mappers}
  for case0 <- cases do
    describe(s"Repeated ${case0.name} ops") {
      for mapper <- mappers do
        describe(s"Composed ${mapper.name}") {
          val case1 = case0.mapEff(mapper)
            it("should be stack safe") {
              mustNotStackOverflow {
                case1.run
              }
            }
        }
    }


private object RepeatedlyTest_Stuff:
  trait Case0:
    enclosing =>
    type Fx
    type This = Case0 { type Fx = enclosing.Fx }
    val name: String
    val comp: Any !! Fx
    def mapEff(mapper: Mapper): This
    def run: Any

  case class Case[F[+_], U](name: String, h: Handler.Free[F, U], comp: Any !! U) extends Case0:
    type Fx = U
    def mapEff(f: Mapper) = copy(comp = f(comp))
    def run = h run comp

  case object FxR extends Reader[Int]
  case object FxW extends Writer[Int]
  case object FxS extends State[Int]
  case object FxC extends Choice

  val cases = List[Case0](
    Case("Reader", FxR.handler(0), FxR.ask),
    Case("Writer", FxW.handler, FxW.tell(111)),
    Case("State", FxS.handler(0), FxS.modify(_ + 1)),
    Case("Choice", FxC.handler, FxC.choose(List(0)))
  )

  abstract class Mapper(val name: String) {
    def apply[U](comp: Any !! U): !![?, U]
  }

  val mappers =
    def many[A, U](comp: A !! U) = Vector.fill(CanStackOverflow.TooBigForStack)(comp)
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
