---
layout: docs
title:  "Effect example: Flip"
---

# Effect example: Flip

Flip effect seems to be the "Hello world" of the Algebraic Effects literature.

This is how it looks in Turbolift:

## Definition

### 1. Imports

```scala mdoc
import turbolift.{!!, Signature, Effect, Handler}
```

### 2. Define the signature

```scala mdoc
trait FlipSignature extends Signature:
  def flip: Boolean !@! ThisEffect
  def fail: Nothing !@! ThisEffect
```

### 3. Define the effect type

```scala mdoc
trait FlipEffect extends Effect[FlipSignature] with FlipSignature: 
  // Boilerplate:
  final override def flip = perform(_.flip)
  final override def fail = perform(_.fail)

  // Auxiliary operations:
  final def plus[A, U <: ThisEffect](lhs: => A !! U, rhs: => A !! U): A !! U =
    flip >>= (if _ then lhs else rhs)

  final def select[A](as: Iterable[A]): A !! ThisEffect =
    if as.isEmpty
    then fail
    else plus(!!.pure(as.head), select(as.tail))
```

The auxiliary operations, `plus` and `select`, are not declared in the signature.
That's because they don't need dynamic semantics, provided by handlers.
They are defined entirely in terms of `flip` and `fail`.


### 4. Define a handler

Or better, **two** handlers:

```scala mdoc
extension (fx: FlipEffect)
  def findAll =
    new fx.Free.Stateless[Vector] with fx.Sequential with FlipSignature:
      override def onReturn[A](a: A) = !!.pure(Vector(a))

      override def fail = _ => !!.pure(Vector())

      override def flip = k =>
        for
          as <- k(true)
          bs <- k(false)
        yield as ++ bs

    .toHandler
```

```scala mdoc
extension (fx: FlipEffect)
  def findFirst =
    new fx.Free.Stateless[Option] with fx.Sequential with FlipSignature:
      override def onReturn[A](a: A) = !!.pure(Some(a))

      override def fail = _ => !!.pure(None)

      override def flip = k =>
        k(true).flatMap {
          case None => k(false)
          case some => !!.pure(some)
        }

    .toHandler
```

---

## Usage

### Instantiate the effect

```scala mdoc
case object MyFlip extends FlipEffect

// Optional:
type MyFlip = MyFlip.type
```

### Run a program using the effect & handlers

```scala mdoc
def isOdd(x: Int) = x % 2 == 1

val program =
  for
    x <- MyFlip.select(1 to 4)
    _ <- !!.when(isOdd(x))(MyFlip.fail)
    y <- MyFlip.select('a' to 'b')
  yield s"$x$y"

val result1 = program.handleWith(MyFlip.findAll).run

val result2 = program.handleWith(MyFlip.findFirst).run
```