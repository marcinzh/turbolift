[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.marcinzh/turbolift-core_3/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.marcinzh/turbolift-core_3)  [![javadoc](https://javadoc.io/badge2/io.github.marcinzh/turbolift-core_3/javadoc.svg)](https://javadoc.io/doc/io.github.marcinzh/turbolift-core_3)

# Turbolift: Algebraic Effects for Scala 3.

Visit the [microsite](https://marcinzh.github.io/turbolift) for description.

# Example

Runnable with `scala-cli`

```scala
//> using scala "3.3.3"
//> using dep "io.github.marcinzh::turbolift-core:0.92.0"
import turbolift.!!
import turbolift.effects.{Reader, State, Error}

@main def main =
  case object MyReader extends Reader[Int]
  case object MyState extends State[Int]
  case object MyError extends Error[String]

  val program =
    for
      a <- MyState.get
      b <- MyReader.ask
      c <-
        if b != 0
        then !!.pure(a / b)
        else MyError.raise(s"Tried to divide $a by zero")
      _ <- MyState.put(c)
    yield ()

  val result =
    program
    .handleWith(MyState.handler(100).justState)
    .handleWith(MyReader.handler(3))
    .handleWith(MyError.handler)
    .run

  println(result) // Right(((),33))
```

&nbsp;

Same, but with [bindless](modules/bindless) syntax extension:

```scala
//> using scala "3.3.3"
//> using dep "io.github.marcinzh::turbolift-core:0.92.0"
//> using dep "io.github.marcinzh::turbolift-bindless:0.92.0"
import turbolift.!!
import turbolift.effects.{Reader, State, Error}
import turbolift.bindless._

@main def main =
  case object MyReader extends Reader[Int]
  case object MyState extends State[Int]
  case object MyError extends Error[String]

  val program =
    `do`:
      val a = MyState.get.!
      val b = MyReader.ask.!
      val c =
        if b != 0
        then a / b
        else MyError.raise(s"Tried to divide $a by zero").!
      MyState.put(c).!

  val result =
    program
    .handleWith(MyState.handler(100))
    .handleWith(MyReader.handler(3))
    .handleWith(MyError.handler)
    .run

  println(result) // Right(((),33))
```
