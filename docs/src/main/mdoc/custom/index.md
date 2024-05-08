---
layout: docs
title:  Defining your own effects and handlers
---


# Defining your own effects and handlers

### 1. Define a signature for your effect

```scala mdoc
import turbolift.{!!, Signature}

trait GoogleSignature extends Signature:
  def countPicturesOf(topic: String): Int !! ThisEffect
```

### What's `ThisEffect`?

It's an abstract member type declared in `Signature`. 
`ThisEffect` provides polymorphism essential for type-level separation between effect's interface (`Effect`) and effect's implementation (`Interpreter`).
Both `Effect` and `Interpreter` inherit from `Signature`.

- On the interface side, `ThisEffect` means a **self-reference** to the effect being defined. It's defined as alias of `this.type`.

- On the implementation side, `ThisEffect` defined as type-level set of **dependencies** of the effect being implemented.

We can compare this to Tagless Final style:

- In Tagless Final style we abstract over the entire monad type: `F[_]`

- In Turbolift our monad type is concrete: `!![_, _]`. Instead, we abstact only over the second parameter of the monad.

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

- See [Flip effect](flip.html#4-define-a-handler) as an example of using **delimited continuations**.

- See [File System](file_sys.html#4-define-a-handler) as an example of **reinterpretation** (implementing an  effect interms of other effects)

Once the interpreter is defined, we can obtain a handler from it, using `toHandler` method.

### More Information

See the source
of predefined [effects](https://github.com/marcinzh/turbolift/tree/master/modules/core/src/main/scala/turbolift/effects)
and their predefined [handlers](https://github.com/marcinzh/turbolift/tree/master/modules/core/src/main/scala/turbolift/handlers).
