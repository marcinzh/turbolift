package turbolift.effects
import scala.io.AnsiColor
import turbolift.{!!, Signature, Effect}
import turbolift.handlers.consoleHandler


trait ConsoleSignature extends Signature:
  def readChar: Option[Char] !@! ThisEffect
  def readLine: String !@! ThisEffect
  def print(text: String): Unit !@! ThisEffect
  def printErr(text: String): Unit !@! ThisEffect
  def printLine(text: String): Unit !@! ThisEffect
  def printLineErr(text: String): Unit !@! ThisEffect


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


object ConsoleEffect:
  extension (fx: ConsoleEffect)
    /** Default handler for this effect. */
    def handler: fx.ThisHandler.FromId.ToId[IO] = fx.consoleHandler


/** Predefined instance of this effect. */
case object Console extends ConsoleEffect
type Console = Console.type
