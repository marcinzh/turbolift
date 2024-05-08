package turbolift.handlers
import java.lang.{System => JConsole}
import java.io.{BufferedReader, InputStreamReader}
import turbolift.effects.{ConsoleSignature, ConsoleEffect, IO}
import turbolift.Extensions._


extension (fx: ConsoleEffect)
  def consoleHandler: fx.ThisHandler[Identity, Identity, IO] =
    IO(new BufferedReader(new InputStreamReader(JConsole.in))) >>=! { breader =>
      new fx.impl.Proxy[IO] with ConsoleSignature:
        override def readChar: Option[Char] !@! ThisEffect = IO.blocking { val n = breader.nn.read(); Option.when(n >= 0)(n.toChar) }
        override def readLine: String !@! ThisEffect = IO.blocking(breader.nn.readLine().nn)
        override def print(text: String): Unit !@! ThisEffect = IO.blocking(JConsole.out.nn.print(text))
        override def printErr(text: String): Unit !@! ThisEffect = IO.blocking(JConsole.err.nn.print(text))
        override def printLine(text: String): Unit !@! ThisEffect = IO.blocking(JConsole.out.nn.println(text))
        override def printLineErr(text: String): Unit !@! ThisEffect = IO.blocking(JConsole.err.nn.println(text))
      .toHandler
    }
