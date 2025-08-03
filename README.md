[![Maven Central Version](https://img.shields.io/maven-central/v/io.github.marcinzh/turbolift-core_3)](https://mvnrepository.com/artifact/io.github.marcinzh/turbolift-core) [![javadoc](https://javadoc.io/badge2/io.github.marcinzh/turbolift-core_3/javadoc.svg)](https://javadoc.io/doc/io.github.marcinzh/turbolift-core_3)

# Turbolift: Algebraic Effects for Scala 3.

Visit the [microsite](https://marcinzh.github.io/turbolift) for description.


See also:
| Project | Description |
|---|---|
| [DaaE](https://github.com/marcinzh/daae) | Demo: a debugger implemented as an effect |
| [Spot](https://github.com/marcinzh/spot) | Cats-Effect instances for Turbolift's `IO` effect |
| [Enterprise](https://github.com/marcinzh/enterprise) | HTTP server implemented using Turbolift's effects |
| [Beam](https://github.com/marcinzh/beam) | Streams implemented with Turbolift's effects |
| [Effect Zoo](https://github.com/marcinzh/effect-zoo) | Microbenchmark suite for several effect systems, including Turbolift |

## Example

Runnable with [`scala-cli`](https://scala-cli.virtuslab.org/).

> [!IMPORTANT]
> Turbolift requires **Java 11** or newer.

```scala
//> using scala "3.3.6"
//> using dep "io.github.marcinzh::turbolift-core:0.118.0"
import turbolift.!!
import turbolift.effects.{Reader, State, Error}

@main def main =
  val program =
    for
      a <- State.get[Int]
      b <- Reader.ask[Int]
      c <-
        if b != 0
        then !!.pure(a / b)
        else Error.raise(s"Tried to divide $a by zero")
      _ <- State.put(c)
    yield ()

  val result =
    program
    .handleWith(State.handler(100))
    .handleWith(Reader.handler(3))
    .handleWith(Error.handler)
    .run

  println(result) // Right(((),33))
```

&nbsp;

Same, but with [bindless](modules/bindless) syntax extension (`async/await` alike):

```scala
//> using scala "3.3.6"
//> using dep "io.github.marcinzh::turbolift-core:0.118.0"
//> using dep "io.github.marcinzh::turbolift-bindless:0.118.0"
import turbolift.!!
import turbolift.effects.{Reader, State, Error}
import turbolift.bindless._

@main def main =
  val program =
    `do`:
      val a = State.get[Int].!
      val b = Reader.ask[Int].!
      val c =
        if b != 0
        then a / b
        else Error.raise(s"Tried to divide $a by zero").!
      State.put(c).!

  val result =
    program
    .handleWith(State.handler(100))
    .handleWith(Reader.handler(3))
    .handleWith(Error.handler)
    .run

  println(result) // Right(((),33))
```

&nbsp;

Same as the first example, but using monomorphized effect instances:
- 👍 Better for modularity (see [Labeled Effects](https://marcinzh.github.io/turbolift/advanced/labelled.html)).
- 👍 QOL: Helps type inference and improves clarity of error messages.
- 👎 Slightly more verbose, since effect instance definitions are required (e.g. `case object MyEffect extends SomeEffect`).

```scala
//> using scala "3.3.6"
//> using dep "io.github.marcinzh::turbolift-core:0.118.0"
import turbolift.!!
import turbolift.effects.{ReaderEffect, StateEffect, ErrorEffect}

@main def main =
  // 👉 Definitions of custom effect instances:
  case object MyReader extends ReaderEffect[Int]
  case object MyState extends StateEffect[Int]
  case object MyError extends ErrorEffect[String]

  val program =
    for
      a <- MyState.get   // 👉 No need for explicit type parameter: `get[Int]`
      b <- MyReader.ask  // 👉 No need for explicit type parameter: `ask[Int]`
      c <-
        if b != 0
        then !!.pure(a / b)
        else MyError.raise(s"Tried to divide $a by zero")
      _ <- MyState.put(c)
    yield ()

  val result =
    program
    .handleWith(MyState.handler(100))
    .handleWith(MyReader.handler(3))
    .handleWith(MyError.handler)
    .run

  println(result) // Right(((),33))
```

&nbsp;

See also [examples](modules/examples/src/main/scala/examples/) folder. Runnable with `sbt`:
```sh
sbt examples/run
```


## Usage in SBT

```scala
libraryDependencies += "io.github.marcinzh" %% "turbolift-core" % "0.118.0"
```

Optionally, for the [bindless](modules/bindless) syntax extension:

```scala
libraryDependencies += "io.github.marcinzh" %% "turbolift-bindless" % "0.118.0"
```
