package turbolift.effects
import java.lang.{System => JConsole}
import java.io.{BufferedReader, InputStreamReader}
import scala.io.AnsiColor
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._


trait ConsoleSignature extends Signature:
  def readChar: Option[Char] !! ThisEffect
  def readLine: String !! ThisEffect
  def print(text: String): Unit !! ThisEffect
  def printErr(text: String): Unit !! ThisEffect
  def printLine(text: String): Unit !! ThisEffect
  def printLineErr(text: String): Unit !! ThisEffect


trait ConsoleEffect extends Effect[ConsoleSignature] with ConsoleSignature with AnsiColor:
  enclosing =>
  final override def readChar: Option[Char] !! this.type = perform(_.readChar)
  final override def readLine: String !! this.type = perform(_.readLine)
  final override def print(text: String): Unit !! this.type = perform(_.print(text))
  final override def printErr(text: String): Unit !! this.type = perform(_.printErr(text))
  final override def printLine(text: String): Unit !! this.type = perform(_.printLine(text))
  final override def printLineErr(text: String): Unit !! this.type = perform(_.printLineErr(text))

  final def readln = readLine
  final def println(text: String) = printLine(text)
  final def printlnErr(text: String) = printLineErr(text)

  /** Predefined handlers for this effect. */
  object handlers:
    /** Invokes `java.lang.System` console functions with `IO.blocking`. */
    def blocking: Handler[Identity, Identity, enclosing.type, IO] =
      IO(new BufferedReader(new InputStreamReader(JConsole.in))).flatMapHandler: breader =>
        new impl.Proxy[IO] with ConsoleSignature:
          override def readChar: Option[Char] !! ThisEffect = IO.blocking { val n = breader.nn.read(); Option.when(n >= 0)(n.toChar) }
          override def readLine: String !! ThisEffect = IO.blocking(breader.nn.readLine().nn)
          override def print(text: String): Unit !! ThisEffect = IO.blocking(JConsole.out.nn.print(text))
          override def printErr(text: String): Unit !! ThisEffect = IO.blocking(JConsole.err.nn.print(text))
          override def printLine(text: String): Unit !! ThisEffect = IO.blocking(JConsole.out.nn.println(text))
          override def printLineErr(text: String): Unit !! ThisEffect = IO.blocking(JConsole.err.nn.println(text))
        .toHandler

    /** Invokes `java.lang.System` console functions with `IO.sync`. */
    def nonBlocking: Handler[Identity, Identity, enclosing.type, IO] =
      IO(new BufferedReader(new InputStreamReader(JConsole.in))).flatMapHandler: breader =>
        new impl.Proxy[IO] with ConsoleSignature:
          override def readChar: Option[Char] !! ThisEffect = IO { val n = breader.nn.read(); Option.when(n >= 0)(n.toChar) }
          override def readLine: String !! ThisEffect = IO(breader.nn.readLine().nn)
          override def print(text: String): Unit !! ThisEffect = IO(JConsole.out.nn.print(text))
          override def printErr(text: String): Unit !! ThisEffect = IO(JConsole.err.nn.print(text))
          override def printLine(text: String): Unit !! ThisEffect = IO(JConsole.out.nn.println(text))
          override def printLineErr(text: String): Unit !! ThisEffect = IO(JConsole.err.nn.println(text))
        .toHandler


/** Predefined instance of this effect. */
case object Console extends ConsoleEffect:
  export handlers.{blocking => handler}

type Console = Console.type
