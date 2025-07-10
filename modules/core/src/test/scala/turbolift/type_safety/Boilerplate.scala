package turbolift.type_safety
import scala.annotation.experimental
import org.specs2.mutable._
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._


@experimental
object MacroSafeSpace:
  object EffectDefinition:
    trait ExampleSig extends Signature:
      def foo: Int !! ThisEffect
      def bar(x: Int): String !! ThisEffect
      // def qux[T](x: T, n: Int): List[T] !! ThisEffect

    val ExampleEffect = Effect.boilerplate[ExampleSig]


  object HandlerDefinition:
    import MacroSafeSpace.EffectDefinition.{ExampleSig, ExampleEffect}

    val theHandler: Handler[Identity, Identity, ExampleEffect.type, Any] =
      new ExampleEffect.impl.Proxy[Any] with ExampleSig:
        override def foo: Int !! ThisEffect = 42.pure_!!
        override def bar(x: Int): String !! ThisEffect = x.toString.mkString(".").pure_!!
        // override def qux[T](x: T, n: Int): List[T] !! ThisEffect = List.fill(n)(x).pure_!!
      .toHandler


@experimental
class BoilerplateTest extends Specification:
  "test" >>{
    import MacroSafeSpace.EffectDefinition.ExampleEffect
    import MacroSafeSpace.HandlerDefinition.theHandler

    ExampleEffect.foo
    .**!(ExampleEffect.bar(123))
    .handleWith(theHandler)
    .run.===((42, "1.2.3"))
  }
