:construction: :construction: Work In Progress :construction: :construction:

# Turbolift

A functional effect system. An alternative to both Eff monad and MTL. Supports higher order effects.

TODO: Insert overly enthusiastic introduction to Turbolift 's capabilities.

```scala
libraryDependencies += "io.github.marcinzh" %% "turbolift-core" % "0.27.0"
```


# Example
```scala
import turbolift.!!
import turbolift.std_effects.{Reader, State, Error}

object Main extends App {
  // Declare some effects:
  case object MyReader extends Reader[Int]
  case object MyState extends State[Int]
  case object MyError extends Error[String]

  // Create a monadic computation using those effects:
  val computation = for {
    a <- MyState.get
    b <- MyReader.ask
    c <- {
      if (b != 0) 
        !!.pure(a / b)
      else 
        MyError.raise(s"Tried to divide $a by zero")
    }
    _ <- MyState.put(c)
  } yield ()

  // Create a handler for the above computation, by composing
  // individual handlers of each requested effect:
  val handler = MyState.handler(100).exec &&&! MyReader.handler(3) &&&! MyError.handler

  // Execute the computation using the handler:
  val result = computation.runWith(handler)

  println(result) // prints "Right(33)"
}
```
