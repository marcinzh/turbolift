:construction: :construction: Work In Progress :construction: :construction:

[![javadoc](https://javadoc.io/badge2/io.github.marcinzh/turbolift-core_3/javadoc.svg)](https://javadoc.io/doc/io.github.marcinzh/turbolift-core_3) 

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

@main def main =
  // Declare some effects:
  case object MyReader extends Reader[Int]
  case object MyState extends State[Int]
  case object MyError extends Error[String]

  // Create a monadic computation using those effects:
  val computation =
    for
      a <- MyState.get
      b <- MyReader.ask
      c <- {
        if b != 0
        then !!.pure(a / b)
        else MyError.raise(s"Tried to divide $a by zero")
      }
      _ <- MyState.put(c)
    yield ()

  // Handle the effects and run the computation:
  val result = computation
    .handleWith(MyState.handler(100).exec)
    .handleWith(MyReader.handler(3))
    .handleWith(MyError.handler)
    .run
 
  println(result) // prints "Right(33)"
```
