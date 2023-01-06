---
layout: docs
title:  Algebraic Effects for Scala 3
---

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.marcinzh/turbolift-core_3/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.marcinzh/turbolift-core_3)  [![javadoc](https://javadoc.io/badge2/io.github.marcinzh/turbolift-core_3/javadoc.svg)](https://javadoc.io/doc/io.github.marcinzh/turbolift-core_3) &nbsp; 🚧 WIP 🚧

# Turbolift: Algebraic Effects for Scala 3

---


## Highlights

&nbsp;

### ⭐ Expressive power

Turbolift supports constructs rarely found in other effect systems,
or in new programming languages with native support for Algebraic Effects. See [Advanced Features](advanced/index.html).

&nbsp;

### ⭐ High performance

Excerpt from [Effect Zoo](https://github.com/marcinzh/effect-zoo) microbenchmark results:

![image](img/bench-cdown.png)

![image](img/bench-sumh.png)

&nbsp;

### ⭐ Lightweight syntax

```scala mdoc
import turbolift.!!
import turbolift.effects.{Reader, State, Error}

case object MyReader extends Reader[Int]
case object MyState extends State[Int]
case object MyError extends Error[String]

val program =
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

val result = program
  .handleWith(MyState.handler(100).justState)
  .handleWith(MyReader.handler(3))
  .handleWith(MyError.handler)
  .run
 
```

---

&nbsp;

## Usage

```scala
libraryDependencies += "io.github.marcinzh" %% "turbolift-core" % "@VERSION@"
```

&nbsp;

## Credits

- Turbolift's syntax and typing of effects and handlers evolved from the predecessor project: [Skutek](https://github.com/marcinzh/skutek) (Eff monad).

- The monad of delimited continuations is inspired by [Scala Effekt](http://b-studios.de/scala-effekt).

- IO related parts (WIP) are inspired by [Cats Effect](https://github.com/typelevel/cats-effect) and [ZIO](https://github.com/zio/zio).
