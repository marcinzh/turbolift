[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.marcinzh/turbolift-core_3/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.marcinzh/turbolift-core_3)  [![javadoc](https://javadoc.io/badge2/io.github.marcinzh/turbolift-core_3/javadoc.svg)](https://javadoc.io/doc/io.github.marcinzh/turbolift-core_3)

# Turbolift

Extensible Effect System for Scala 3.

:construction: WIP :construction:

## Features

- Have the cake, and eat the cake.
  |  | MTL | Eff monad | ZIO | Turbolift |
  | :--- | :---: | :---: | :---: | :---: |
  | Higher order effects | :heavy_check_mark: | :x: | :heavy_check_mark: | :heavy_check_mark: |
  | Localy handled effects | :x: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
  | Nondeterminism | :heavy_check_mark: | :heavy_check_mark: | :x: | :heavy_check_mark: |
  | *"One monad, to rule them all"* | :x: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |

- 🦄 🌈 Extremely rarely[*] supported aspect of modularity: 
  - In Idris, known as [Effect Labelling](https://docs.idris-lang.org/en/latest/effects/state.html#labelled-effects).
  - In Helium, known as [Effect Instances](https://www.youtube.com/watch?v=6lv_E-CjGzg).
  
  TL;DR: You can use multiple instances of the same kind of effect (e.g. 2 `State` effects) at the same time. Turbolift uses Scala's singleton types to distinguish instances of effects. See the signature of `inMemoryFileSystemHandler` in the example below.

  [*] AFAIK, no effect system in Scala or Haskell supports this. Neither do any of new (research) programming languages with built-in algebraic effects, except Idris and Helium.

- Lightweight syntax.
 
- High performance.

## Examples

### Basic example

```scala
import turbolift.!!
import turbolift.std_effects.{Reader, State, Error}

@main def main =
  case object MyReader extends Reader[Int]
  case object MyState extends State[Int]
  case object MyError extends Error[String]

  val program =
    for
      a <- MyState.get
      b <- MyReader.ask
      c <- {
        if b != 0
        then !!.pure(a / b)
        else MyError.raise(s"Tried to divide $a by zero")
      }
      _ <- MyState.put(c)
    yield ()

  val result = program
    .handleWith(MyState.handler(100).justState)
    .handleWith(MyReader.handler(3))
    .handleWith(MyError.handler)
    .run
 
  println(result) // prints "Right(33)"
```

### File System example

```scala
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.std_effects.{State, Error}

//======== Effectful API definition ======== 

trait FileSystemSignature extends Signature:
  def readFile(path: String): String !@! (ThisEffect & FileError)
  def writeFile(path: String, contents: String): Unit !@! ThisEffect

trait FileSystem extends Effect[FileSystemSignature] with FileSystemSignature:
  final override def readFile(path: String) = perform(_.readFile(path))
  final override def writeFile(path: String, contents: String) = perform(_.writeFile(path, contents))

case object FileError extends Error[FileErrorCause]
type FileError = FileError.type

enum FileErrorCause:
  case NoSuchFile(path: String)

  def message = this match
    case NoSuchFile(path) => s"No such file found: $path"

//======== Handler implementation ======== 

def inMemoryFileSystemHandler[Fx <: FileSystem](fx: Fx): Handler.FreeId[fx.type] =
  case object InternalStorage extends State[Map[String, String]]
  type InternalStorage = InternalStorage.type

  new fx.Proxy[InternalStorage] with FileSystemSignature:
    override def readFile(path: String): String !@! (ThisEffect & FileError) =
      InternalStorage.gets(_.get(path)).flatMap {
        case Some(contents) => !!.pure(contents)
        case None => FileError.raise(FileErrorCause.NoSuchFile(path))
      }

    override def writeFile(path: String, contents: String): Unit !@! ThisEffect =
      InternalStorage.modify(_.updated(path, contents))

  .toHandler
  .provideWith(InternalStorage.handler(Map()).dropState)

//======== Usage ======== 

@main def main =
  case object MyFS extends FileSystem
  type MyFS = MyFS.type

  val program =
    for
      _ <- MyFS.writeFile("hello.txt", "Hello world!")
      contents <- MyFS.readFile("hello.txt")
      _ <- !!.impure(println(contents))
    yield ()

  program
    .handleWith(inMemoryFileSystemHandler(MyFS))
    .handleWith(FileError.handler.getOrDie(_.message))
    .run
    
  // prints "Hello world!"
```

## Usage

```scala
libraryDependencies += "io.github.marcinzh" %% "turbolift-core" % "0.27.0"
```

## Documentation

Rudimentary, in the form of javadoc for the 4 key Turbolift's types:
- [Computation](https://javadoc.io/static/io.github.marcinzh/turbolift-core_3/0.27.0/turbolift/Computation.html) (aliased by `!!`)
- [Signature](https://javadoc.io/static/io.github.marcinzh/turbolift-core_3/0.27.0/turbolift/Signature.html)
- [Effect](https://javadoc.io/static/io.github.marcinzh/turbolift-core_3/0.27.0/turbolift/Effect.html)
- [Handler](https://javadoc.io/static/io.github.marcinzh/turbolift-core_3/0.27.0/turbolift/Handler.html)


## Why exclusively Scala 3?

- ❤️ The new syntax.

- ❤️❤️ Higher-rank types (`[X] => ...`) and functions. Used mostly by `Handler`.

- ❤️❤️❤️ Undocumented advancements in Scala's type inference made usage of `Handler`s much better.
When applying `.handleWith`, Scala compiler can now infer subtraction of type-level sets of effects, 
without any explicit type hints. This even works in chaining `.handleWith`s of handlers, that have internal dependencies between them.

  The ergonomics is similar to that of ZIO 2's `provide`, which relies on reflection and macros.

## The Future

Turbolift's internal engine will abandon monad transformers. Instead, it will use multi prompt delimited continuations, a technique learned from [Scala Effekt](https://github.com/b-studios/scala-effekt). The change won't affect Turbolift's user interface, except for how user defines his/her own `Flow` `Interpreter`s. `Proxy` `Interpreter`s (such as `inMemoryFileSystemHandler` above) will be unaffected.

Anticipated benefits:

- Simpler API for defining [Flow](file:///home/luzer/prog/scala/turbolift/modules/core/target/scala-3.1.1/api/turbolift/internals/interpreter/Interpreter$$Flow.html) effects. Never have to implement `flatMap` again.

- Even better performance.

- Improving Turbolift's behavior with respect to the matters discussed in [The Effect Semantics Zoo](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md). Currently, Turbolift exhibits similar limitations to those of other effect systems based on monad transformers.

