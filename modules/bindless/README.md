
# Bindless Syntax

Provided by [dotty-cps-async](https://github.com/rssh/dotty-cps-async).

- Instead of `async` use ``` `do` ```.
- Instead of `await` use **postfix** `!` operator.

### Example:

Using Scala's `for` comprehension:

```scala
for
  a <- foo
  b <- bar
yield a + b
```

Using the original `dotty-cps-async` syntax, it could be rewritten as:

```scala
async:
  await(foo) + await(bar)
```

In Turbolift's `bindless` syntax though, it should be rewritten as:

```scala
`do`:
  foo.! + bar.!
```


# Attribution

This module contains code adapted from the `kyo-direct` module in the Kyo project, which is
licensed under the Apache License 2.0.

Modifications have been made to the original code so that it works with Turbolift's `Computation` monad.

**Original author**: Flavio Brasil  
**Location of the original**: [getkyo/kyo-direct](https://github.com/getkyo/kyo/blob/75c0ae7cf18ad5671ba278d7bbc953689ccd56b3/kyo-direct)  
**Date of modification**: 13 July 2024  

For the full license text, see the `LICENSE-APACHE` file in this directory.
