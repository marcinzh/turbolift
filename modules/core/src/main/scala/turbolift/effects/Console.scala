package turbolift.effects
import scala.io.AnsiColor
import turbolift.{!!, Signature, Effect}
import turbolift.io.IO
import turbolift.effects.default_handlers.consoleHandler


trait ConsoleSig extends Signature:
  def readLine: String !@! ThisEffect
  def print(text: String): Unit !@! ThisEffect
  def printErr(text: String): Unit !@! ThisEffect
  def printLine(text: String): Unit !@! ThisEffect
  def printLineErr(text: String): Unit !@! ThisEffect


trait ConsoleEffect extends Effect[ConsoleSig] with ConsoleSig with AnsiColor:
  override def readLine: String !! this.type = perform(_.readLine)
  override def print(text: String): Unit !! this.type = perform(_.print(text))
  override def printErr(text: String): Unit !! this.type = perform(_.printErr(text))
  override def printLine(text: String): Unit !! this.type = perform(_.printLine(text))
  override def printLineErr(text: String): Unit !! this.type = perform(_.printLineErr(text))

  final def readln = readLine
  final def println(text: String) = printLine(text)
  final def printlnErr(text: String) = printLineErr(text)

  /** Default handler for this effect. */
  def handler: ThisHandler.Id[IO] = consoleHandler.apply(this)

/** Predefined instance of this effect. */
case object Console extends ConsoleEffect
type Console = Console.type
