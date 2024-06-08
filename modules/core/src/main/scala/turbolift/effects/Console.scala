package turbolift.effects
import scala.io.AnsiColor
import turbolift.{!!, Signature, Effect}
import turbolift.handlers.{consoleHandler_blocking, consoleHandler_nonBlocking}


trait ConsoleSignature extends Signature:
  def readChar: Option[Char] !! ThisEffect
  def readLine: String !! ThisEffect
  def print(text: String): Unit !! ThisEffect
  def printErr(text: String): Unit !! ThisEffect
  def printLine(text: String): Unit !! ThisEffect
  def printLineErr(text: String): Unit !! ThisEffect


trait ConsoleEffect extends Effect[ConsoleSignature] with ConsoleSignature with AnsiColor:
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
    def blocking: ThisHandler[Identity, Identity, IO] = ConsoleEffect.this.consoleHandler_blocking
    def nonBlocking: ThisHandler[Identity, Identity, IO] = ConsoleEffect.this.consoleHandler_nonBlocking


/** Predefined instance of this effect. */
case object Console extends ConsoleEffect:
  export handlers.{blocking => handler}

type Console = Console.type
