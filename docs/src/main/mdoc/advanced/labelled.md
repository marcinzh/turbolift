---
layout: docs
title:  Labelled Effects
---

# Labelled Effects

Ability to label effects, is very rarely found feature in effect systems.
That's a shame, because without it, there is no true modularity.

Related reading:

- Idris language - [Labelled Effects](https://docs.idris-lang.org/en/latest/effects/state.html#labelled-effects)
- Helium language - [Effect Instances](https://bitbucket.org/pl-uwr/helium/wiki/popl20/Tutorial_Instances)

In Idris and Helium, effect labelling is optional.

In Turbolift, effects are always uniquely labeled, thanks to Scala's singleton types: 

```scala mdoc
import turbolift.effects.Error

// Unique value:
case object MyError extends Error[String]

// Unique type:
type MyError = MyError.type
```

There is nothing stopping us from instantiating given effect more than once:

```scala mdoc:reset
import turbolift.effects.Error

case object MyError1 extends Error[String]
case object MyError2 extends Error[String]

type MyError1 = MyError1.type
type MyError2 = MyError2.type
```

Each instance is a fully independent effect:
- They may be instantiated with different type parameters (e.g. `State[Int]` and `State[String]`).
- They may be used together in a computation.
The type of the computation will reflect this, showing 2 distinct effects (e.g. `MyError1 & MyError2`)
- They may be handled at different points in program, and with different handlers.


### Example: Using 2 State effects at the same time

```scala mdoc:reset
import turbolift.!!
import turbolift.effects.State

case object Foo extends State[Int]
case object Bar extends State[Int]

val program =
  for
    x <- Foo.get
    _ <- Bar.put(-x)
  yield ()

val result = program
  .handleWith(Foo.handler(42))
  .handleWith(Bar.handler(1337))
  .run
```

### Performance

Performance penalty for using multiple effects in Turbolift, is small.
Here is comparison with old version of Turbolift, that was based on Monad Transformers:

![image](../img/bench-mtrans.png)
