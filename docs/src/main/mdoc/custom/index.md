---
layout: docs
title:  Defining your own effects and handlers
---


# Defining your own effects and handlers

### 1. Define a signature for your effect

```scala mdoc
import turbolift.Signature

trait GoogleSignature extends Signature:
  def countPicturesOf(topic: String): Int !@! ThisEffect
```

### 😱 `!@!`

`!@![_, ThisEffect]` is somewhat similar to `F[_]` from Tagless Final.

The main difference, is that in Turbolift this abstraction is **short lived**.
It's introduced in `Signature`, and it vanishes in its direct subtrait, `Effect`.

Both `!@!` and `ThisEffect` are defined as abstract type members of `Signature`.
In the next step, once we inherit our signature from an `Effect`, they become (automatically) concretized:
- `!@![_, _]` becomes an alias of `Computation[_, _]` (just like `!!`)
- `ThisEffect` becomes an alias of `this.type`

Effectively, when writing custom signature,
`A !@! ThisEffect` should be regarded as `A !! this.type`, in temporary disguise.

### 2. Define your effect type

This step is mostly **mechanical**. 

For each abstract method we have defined in the signature, we must provide boilerplate implementation stub,
using `perform` (provided by `Effect`).

```scala mdoc
import turbolift.Effect

trait Google extends Effect[GoogleSignature] with GoogleSignature:
  // Boilerplate:
  final override def countPicturesOf(topic: String) = perform(_.countPicturesOf(topic))
```

### ⚠️ Notice:

The signature is used here **twice**: first as the type parameter and second as the super trait.

&nbsp;

Hopefully, with future version of Scala compiler,
it will be possible to automatically generate the boilerplate methods,
by using annotation macro:

```scala
import turbolift.{Effect, effect}

@effect trait Google extends Effect[GoogleSignature] with GoogleSignature
  // No boilerplate methods needed.
```


### 3. Define a handler for your effect

Now it's time to make an important choice. There are 2 ways to assign semantics to our effect's operations:

- With `Flow` interpreter: by using **delimited continuations**.

  See [Flip effect](flip.html#4-define-a-handler) example for details.

- With `Proxy` interpreter: by **delegating to other effects** (a.k.a "reinterpretation").

  See [File System](file_sys.html#4-define-a-handler) example for details.

Once the interpreter is defined, we can obtain a handler from it, using `toHandler` method.


### More Information

See [the source](https://github.com/marcinzh/turbolift/tree/master/modules/core/src/main/scala/turbolift/effects)
of predefined effects and their handlers.