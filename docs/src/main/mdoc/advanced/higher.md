---
layout: docs
title:  Higher Order Effects
---

# Higher Order Effects

a.k.a Scoped Effects.

### HOEs are problematic

- New programming languages with native Algebraic Effects, generally don't support HOEs. Exceptions are:
  - [Frank](https://github.com/frank-lang/frank) language.
  - [Unison](https://www.unison-lang.org/) language, which implements Frank's effect system.

- According to the underlying theory, HOEs are actually [non-algebraic](https://old.reddit.com/r/haskell/comments/ej8fme/unordered_effects/fd00mk2/?context=3) ⚠️**λ**[^1].

- The Eff Monad doesn't support HOEs.

- Monad Transformers do support HOEs. However, there are some known problems. Such as effect's semantics being dependent
on the order of monad transformers in the stack. More info on the subject:
  - [Unresolved challenges of scoped effects](https://old.reddit.com/r/haskell/comments/pywuqg/unresolved_challenges_of_scoped_effects_and_what/) ⚠️**λ**[^1] video.
  - [Effect Semantics Zoo](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md) ⚠️**λ**[^1]

### HOEs in Turbolift

In this example, we run the given program twice, with 2 orderings of handlers:
1. `Error` handled before `State`.
2. `State` handled before `Error`.

We observe consistent behavior: in both cases,
raising the error didn't cause the `State` to reset to it's value from
before the `catchAll` operation.

```scala mdoc
import turbolift.!!
import turbolift.effects.{Error, State}

case object MyError extends Error[String]
case object MyState extends State[Int]

val program =
  MyError.catchAll {
    MyState.put(42) &&!
    MyError.raise("error")
  } {
    case _ => !!.pure("nvm")
  }

val result1 = program
  .handleWith(MyError.handler)
  .handleWith(MyState.handler(0))
  .run

val result2 = program
  .handleWith(MyState.handler(0))
  .handleWith(MyError.handler)
  .run
```

---

[^1]: **Warning:** Haskell code ahead.