:construction: :construction: Work In Progress :construction: :construction:

# Turbolift

A functional effect system. An alternative to both Eff monad and MTL. Supports higher order effects.

TODO: Insert overly enthusiastic introduction to Turbolift 's capabilities.

```scala
resolvers += Resolver.jcenterRepo
libraryDependencies += "com.github.marcinzh" %% "turbolift-core" % "0.9.0"
```


# Example
```scala
import turbolift.abstraction.!!
import turbolift.std_effects.{Reader, State, Except}

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
        !!.pure(a / b)
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
