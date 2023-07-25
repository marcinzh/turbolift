---
layout: docs
title:  Overview
---

# Overview

### The main types of Turbolift, and their roles

- `Computation` - Monad, parametrized by set of effects, a.k.a **"One Monad To Rule Them All"** [^1].
- `Signature` - Trait, where we define our *Algebra/Service/DSL* (as abstract methods).
- `Effect` - Object, through which we can invoke operations of some *Algebra/Service/DSL* (as concrete methods).
- `Interpreter` - Object that assigns semantics to some *Algebra/Service/DSL*. It produces a `Handler`.
- `Handler` - Object that we can use like generalized `try ... catch` expression: to delimit scope of effect(s).

### Computation Usage

A value of type `Computation[+A, -U]` describes a... computation,
that **requests** a set of effects `U`, that need to be **handled**,
before it can return a value of type `A`.

A **type alias** `!![A, U]` is defined for convenient **infix syntax**.

The type-level set of effects is modelled by intersection types:

| Scala type | Meaning as a set of effects | Sample computation type | Same, using infix syntax |
|---|---|---|---|
| `Any`   | ∅         | `Computation[Int, Any]`    | `Int !! Any` |
| `X`     | `X`       | `Computation[Int, X]`      | `Int !! X` |
| `X & Y` | `X` ∪ `Y` | `Computation[Int, X & Y]`  | `Int !! (X & Y)` |

&nbsp;

Usually, Scala compiler can infer the set of effects requested by the computation.
We can see this in [code example](index.html#-lightweight-syntax) on the front page.
The inferred type of `program` indicates,
that it requests 3 effects: `MyReader`, `MyState` and `MyError`.

&nbsp;


Additionally, `!!` is a **value alias** of `Computation`'s companion object:

```scala mdoc
import turbolift.!!

val myComputation1 = !!.unit

val myComputation2 = !!.pure(42)
```

For more information, see [Computation API](https://javadoc.io/static/io.github.marcinzh/turbolift-core_3/@VERSION@/turbolift/Computation.html).

### Effect Usage

To be able to invoke the effect's operations, we need access to an instance of the effect.

We can create such instance ourselves:

```scala mdoc
// State inherits from Effect:
import turbolift.effects.State

// Instantiation:
case object MyState extends State[Int]

// Invoking operation:
val computation = MyState.put(42)
```

For more details, see [Defining your own effects & handlers](custom/index.html) and [Effect labelling](advanced/labelled.html).


### Handler Usage

Application of a handler delimits scope of effect(s).
It also transforms type of the computation. 
In the simplest case, one of effects requested by the computation is eliminated.

```scala
  val myComputation2 = myComputation1.handleWith(myHandler)
```

As soon as all effects are eliminated, the computation's result can be obtained, using `run`:

```scala
  val result = myComputation
    .handleWith(myHandler1)
    .handleWith(myHandler2)
    .handleWith(myHandler3)
    .run
```

...or using `unsafeRun`, if the only effect remaining unhandled, is `IO`.


In general, a handler of type `Handler[F[_], L, N]` represents a
[polymorphic function](https://docs.scala-lang.org/scala3/reference/new-types/polymorphic-function-types.html),
that transforms computations:

```scala
∀ A, M.  Computation[F[A], M ∪ L] => Computation[G[A], M ∪ N]

// Where `F[_]` is either type-level identity, or constant function.
```

Meaning, that application of it, does the following:
- It e**L**iminates  set of effects `L` from incoming computation.
- It i**N**troduces  set of effects `N` into outgoing computation (revealing dependencies of the handler, if there are any).
- It passes a**M**bient set of effects `M` unaffected, from incoming to outgoing computation.
- It applies type constructor `G[_]` to `A`.
- It constraints type returned by the incoming computation to be equal `F[A]`.



In the example below, `myHandler` eliminates single `MyChoice` effect, introduces no effects,
accepts any type of computation (identity), and wraps the result type in `Vector[_]`.

```scala mdoc:silent
import turbolift.Handler
import turbolift.effects.Choice

case object MyChoice extends Choice
type MyChoice = MyChoice.type

val myHandler: Handler[[X] =>> X, Vector, MyChoice, Any] = MyChoice.handler
```

Handlers can be transformed or composed in various ways.

For example, this sequences 3 independent handlers:

```scala
val myHandler123 = myHandler1 &&&! myHandler2 &&&! myHandler3
```

For more operations, see [Handler API](https://javadoc.io/static/io.github.marcinzh/turbolift-core_3/@VERSION@/turbolift/Handler.html).



### Signature & Interpreter Usage

Those 2 types are used only during [Defining your own effects & handlers](custom/index.html).

---

[^1]: Slogan coined by [Eric Torreborre](https://www.youtube.com/watch?v=KGJLeHhsZBo).
