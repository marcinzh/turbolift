---
layout: docs
title:  Bidirectional Effects
---

# Bidirectional Effects

Typically, effect's operations are defined so that they request exactly 1 effect:
the very effect they belong to. This **self-reference** in Turbolift is named `ThisEffect`:

```scala mdoc
import turbolift.{Signature, Effect}

trait FileSystemSignature extends Signature:
  def readFile(path: String): String !@! ThisEffect
  def writeFile(path: String, contents: String): Unit !@! ThisEffect

// Boilerplate:
case object FileSystem extends Effect[FileSystemSignature] with FileSystemSignature:
  final override def readFile(path: String) = perform(_.readFile(path))
  final override def writeFile(path: String, contents: String) = perform(_.writeFile(path, contents))

```

However, in Turbolift it's possible for an operation to also request **other effects**,
in addition to the self-referenced one.

Let's have such another effect defined:
```scala mdoc:reset
import turbolift.effects.Error

case object FileNotFound extends Error[String]
type FileNotFound = FileNotFound.type
```

Now we can modify `readFile` definition, so that it requests `FileNotFound` effect:
```scala mdoc
import turbolift.{Signature, Effect}

trait FileSystemSignature extends Signature:
  def readFile(path: String): String !@! (ThisEffect & FileNotFound)
  def writeFile(path: String, contents: String): Unit !@! ThisEffect

// Boilerplate:
// (unchanged, except omitted types)
case object FileSystem extends Effect[FileSystemSignature] with FileSystemSignature:
  final override def readFile(path: String) = perform(_.readFile(path))
  final override def writeFile(path: String, contents: String) = perform(_.writeFile(path, contents))
```

This establishes **public dependency** of `FileSystem` effect, on `FileNotFound` effect.

Public dependencies differ from **private dependencies** of interpreters/handlers:

- Public dependency manifests early, in the interface (`Signature`).
Private dependency manifests late, in the implementation (`Interpreter` delegating to other effect),
or application (`Handler` with unsatisfied dependency).

- Public dependency doesn't constraint client code
with respect to scopes of both effects (depenent & depenency) and the order of their handling.
In private dependency, the depenent effect must be handled after all of its dependencies are handled.

- Public dependencies can be cyclic.


### See also:

- [Handling Bidirectional Control Flow](https://www.youtube.com/watch?v=RLTEuZNtRCc) video.

- `PingPong` and `YieldReplace` [examples](https://github.com/marcinzh/turbolift/tree/master/modules/examples/src/main/scala/examples)
in Turbolift's repo.