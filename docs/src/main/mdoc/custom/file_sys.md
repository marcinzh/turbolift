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
  def readFile(path: String): String !@! (ThisEffect & FileError)
  def writeFile(path: String, contents: String): Unit !@! ThisEffect
```

Let's also instantiate the custom `FileError` effect, that we used in the signature:

```scala mdoc
case object FileError extends Error[FileErrorCause]
type FileError = FileError.type

enum FileErrorCause:
  case NoSuchFile(path: String)

  def message = this match
    case NoSuchFile(path) => s"No such file found: $path"
```

### 3. Define the effect type

```scala mdoc
trait FileSystemEffect extends Effect[FileSystemSignature] with FileSystemSignature:
  // Boilerplate:
  final override def readFile(path: String) = perform(_.readFile(path))
  final override def writeFile(path: String, contents: String) = perform(_.writeFile(path, contents))
```

### 4. Define a handler

```scala mdoc
extension (fx: FileSystemEffect)
  def inMemoryHandler =
    case object InternalStorage extends State[Map[String, String]]

    new fx.Proxy[InternalStorage.type] with FileSystemSignature:
      override def readFile(path: String) =
        kk => InternalStorage.gets(_.get(path)).flatMap {
          case Some(contents) => !!.pure(contents)
          case None => kk.escape(FileError.raise(FileErrorCause.NoSuchFile(path)))
        }

      override def writeFile(path: String, contents: String) =
        _ => InternalStorage.modify(_.updated(path, contents))

    .toHandler
    .provideWith(InternalStorage.handler(Map()).dropState)
```

The `provideWith` method composes 2 dependent handlers,
such that the latter handles dependency introduced by the former
(`InternalStorage` effect in this case).

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
