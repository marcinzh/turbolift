---
layout: docs
title:  Applicative Effects
---


# Applicative Effects

### Parallellism

Two independent computations can be combined, giving potential for their parallel execution:
```scala
val foobar = foo *! bar
```
The `*!` operator is an alias for `zipPar` method (see [Computation API](https://javadoc.io/static/io.github.marcinzh/turbolift-core_3/@VERSION@/turbolift/Computation.html)).

The possibility of parallelism, depends on **implementation of handlers**.
Parallelism is possible only when **all** handlers in the currently used effect stack,
are implemented to permit parallelism.

- If parallelization is possible, 2 fibers[^1] for `foo` and `bar` are **implicitly** forked.
Upon joining, results of each contributing effect are composed, 
in a similar manner as in composed Applicative Functor[^2].

- If parallelization is not possible, `zipPar` fallbacks to sequential `zip`.

This condition doesn't apply to fibers forked & joined **explicitly** (WIP).

### Parallelizability of predefined handlers

<table>
  <thead>
    <tr>
      <th align="center">Predefined effect</th>
      <th align="center">Predefined handler for this effect</th>
      <th align="center">Is the handler parallelizable?</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>Reader</code></td>
      <td>default</td>
      <td>✅</td>
    </tr>
    <tr>
      <td rowspan="2"><code>Writer</code></td>
      <td><code>local</code></td>
      <td>✅</td>
    </tr>
    <tr>
      <td><code>shared</code></td>
      <td>✅</td>
    </tr>
    <tr>
      <td rowspan="2"><code>State</code></td>
      <td><code>local</code></td>
      <td>❌</td>
    </tr>
    <tr>
      <td><code>shared</code></td>
      <td>✅</td>
    </tr>
    <tr>
      <td rowspan="2"><code>Error</code></td>
      <td><code>first</code></td>
      <td>❌</td>
    </tr>
    <tr>
      <td><code>all</code></td>
      <td>✅</td>
    </tr>
    <tr>
      <td rowspan="2"><code>Choice</code></td>
      <td><code>first</code></td>
      <td>✅</td>
    </tr>
    <tr>
      <td><code>all</code></td>
      <td>✅</td>
    </tr>
    <tr>
      <td rowspan="2"><code>Random</code></td>
      <td><code>local</code></td>
      <td>✅</td>
    </tr>
    <tr>
      <td><code>shared</code></td>
      <td>✅</td>
    </tr>
    <tr>
      <td><code>Console</code></td>
      <td>default</td>
      <td>✅</td>
    </tr>
  </tbody>
</table>

&nbsp;

`State`'s `local` handler is conceptually similar to standard `State` monad.
State updates are chained linearly.
It is inherently impossible to fork-and-join such chain
without having to make arbitrary choice about information loss.
For this reason, this handler prohibits parallellism.
See [Everything you didn't want to know about StateT](https://www.youtube.com/watch?v=KZIN9f9rI34&t=515s) ⚠️**λ**[^3] video for more information.

`Error`'s `first` handler short-circuits the computation on the first raised error.
This handler prohibits parallellism,
because the second computation can't be run,
until it's determined that the first one completes without error.

### Example: Applicative vs. Monadic error

Depending on selected handler, given program will either:

- Attempt to execute both branches **sequentially**, but will stop on the first error.

- Attempt to execute both branches **parallelly**, and will collect both errors.

```scala mdoc
import turbolift.!!
import turbolift.effects.ErrorK

case object MyError extends ErrorK[List, String]

val program = MyError.raise("foo") &! MyError.raise("bar")

val result1 = program.handleWith(MyError.handlers.first).run

val result2 = program.handleWith(MyError.handlers.all).run
```

---

[^1]: Currently, fibers are not exposed to user. 🚧 **WIP** 🚧
[^2]: Turbolift does not use `Applicative` typeclass. It's only mentioned as an analogy.
[^3]: **Warning:** Haskell code ahead.