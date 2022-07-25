[![javadoc](https://javadoc.io/badge2/io.github.marcinzh/turbolift-core_3/javadoc.svg)](https://javadoc.io/doc/io.github.marcinzh/turbolift-core_3) 

# Turbolift

:construction: :construction: Work In Progress :construction: :construction:

A functional effect system. An alternative to both Eff monad and MTL. Supports higher order effects.

TODO: Insert overly enthusiastic introduction to Turbolift 's capabilities.

```scala
libraryDependencies += "io.github.marcinzh" %% "turbolift-core" % "0.27.0"
```

---

# Examples

### Basic example

```scala
import turbolift.!!
import turbolift.std_effects.{Reader, State, Error}

@main def main =
  case object MyReader extends Reader[Int]
  case object MyState extends State[Int]
  case object MyError extends Error[String]

  val computation =
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

  val result = computation
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
