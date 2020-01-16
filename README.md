# Turbolift

:construction: :construction: Work In Progress :construction: :construction:

An alternative to both MTL and Eff monad. Combines advantages of extensible effects (on the frontend) and monad transformers (on the backend). Supports higher order effects.

The frontend is mostly inherited from [Skutek](https://github.com/marcinzh/skutek) - an implementation of Eff monad, using Scala's intersection types for tracking sets of effects used in computations.

The backend doesn't use canonical monad transformer's data types (e.g. `StateT`). Instead, it uses type aliases, encapsulated in effect handler definitions. As a side effect of this design, number of object allocations at runtime is reduced (preliminary microbenchmarking shows x2 performance improvement over Cats monad transformers). Effect handlers though, are defined in terms of canonical monad->monad transformation, with methods such as `lift` and `flatMap`, to fill in (example: definition of `State` effect [here](https://github.com/marcinzh/turbolift/blob/35147d3545f5d7bbcd0d7f1498fedb3d6469dd39/modules/core/src/main/scala/turbolift/std_effects/State.scala#L28-L33)).



# Roadmap

1. Provide missing critical functionality, such as:

   - Stack safety for parallel (applicative-like) composition (tough one).
   
   - Ability to define new effects, by delegation to preexisting effects.

2. Replace `mwords` (a self-made, stop-gap, minimal FP library) with dependency on `cats-core`

3. Explore using preexisting IO monads (Cats-Effect, ZIO, Monix) as the base for Turbolift's monad stack. Currently, Identity and Trampoline are provided as the base monads.

4. Improve performance of larger effect stacks, by coalescing similar kinded effects, adjacent in the stack, into single one, backed by shared data type. This would result in flatter effect stacks. Preliminary microbenchmarking shows potential for x2-3 performance gain, over current Turbolift's equally sized effect stack, but made of fully isolated effects.


# Example
```scala
import turbolift.abstraction._
import turbolift.std_effects._

object Main extends App {
  // Declare some effects:
  case object MyReader extends Reader[Int]
  case object MyState extends State[Int]
  case object MyExcept extends Except[String]

  // Create a monadic computation using those effects:
  val computation = for {
    a <- MyState.get
    b <- MyReader.ask
    c <- {
      if (b != 0) 
        Return(a / b)
      else 
        MyExcept.raise(s"Tried to divide $a by zero")
    }
    _ <- MyState.put(c)
  } yield ()

  // Create a handler for the above computation, by composing
  // individual handlers of each requested effect:
  val handler = MyExcept.handler <<<! MyState.handler(100).exec <<<! MyReader.handler(3)

  // Execute the computation using the handler:
  val result = handler.run(computation)

  println(result) // prints "Right(33)"
}
```
