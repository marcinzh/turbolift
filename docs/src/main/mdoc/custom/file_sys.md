---
layout: docs
title: "Effect example: File System"
---

# Effect example: File System

In the Haskell community, there are many effect systems positioning themselves as an alternative to the mainstream MTL.
The File System effect example is often used as their demonstrator of reinterpretation.

This is how it looks in Turbolift:

## Definition

### 1. Imports

```scala mdoc
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.effects.{State, Error}
```

### 2. Define the signature

```scala mdoc
trait FileSystemSignature extends Signature:
  def readFile(path: String): String !@! ThisEffect
  def writeFile(path: String, contents: String): Unit !@! ThisEffect
```

### 3. Define the effect type

```scala mdoc
trait FileSystemEffect extends Effect[FileSystemSignature] with FileSystemSignature:
  // Boilerplate:
  final override def readFile(path: String) = perform(_.readFile(path))
  final override def writeFile(path: String, contents: String) = perform(_.writeFile(path, contents))
```

### 4. Define a handler

Instantiate custom `FileError` effect, that we will be using in the handler:

```scala mdoc
case object FileError extends Error[FileErrorCause]
type FileError = FileError.type

enum FileErrorCause:
  case NoSuchFile(path: String)

  def message = this match
    case NoSuchFile(path) => s"No such file found: $path"
```

The handler itself:

```scala mdoc
extension (fx: FileSystemEffect)
  def inMemoryHandler =
    // Internal state:
    case object S extends State[Map[String, String]]
    type S = S.type

    // Our proxy depends on 2 effects: `S` and `FileError`
    new fx.impl.Proxy[S & FileError] with FileSystemSignature:
      override def readFile(path: String) =
        S.gets(_.get(path)).flatMap {
          case Some(contents) => !!.pure(contents)
          case None => FileError.raise(FileErrorCause.NoSuchFile(path))
        }

      override def writeFile(path: String, contents: String) =
        S.modify(_.updated(path, contents))

    .toHandler
    .partiallyProvideWith[FileError](S.handler(Map()).dropState)
```

Our handler has 2 dependencies: `S` and `FileError` effects.
Using `partiallyProvideWith` method, we modify the handler in such a way,
that dependency on `S` is removed (satisfied), but dependency on `FileError` remains.
This way, we hide handler's internal state from the outside world. 
Responsibility to handle the error effect though, is passed on the user.

---

## Usage

### Instantiate the effect

```scala mdoc
case object MyFS extends FileSystemEffect

// Optional:
type MyFS = MyFS.type
```

### Run a program using the effect & handler

```scala mdoc
val program =
  for
    _ <- MyFS.writeFile("hello.txt", "Hello world!")
    contents <- MyFS.readFile("hello.txt")
  yield contents

val result = program
  .handleWith(MyFS.inMemoryHandler)
  .handleWith(FileError.handler)
  .run
```
