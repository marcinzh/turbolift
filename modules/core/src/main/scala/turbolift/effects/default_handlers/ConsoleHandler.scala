package turbolift.effects.default_handlers
import java.lang.{System => JConsole}
import java.io.{BufferedReader, InputStreamReader}
import turbolift.effects.{ConsoleSignature, ConsoleEffect, IO}


extension (fx: ConsoleEffect)
  private[effects] def consoleHandler: fx.ThisHandler.Id[IO] =
    IO(new BufferedReader(new InputStreamReader(JConsole.in))) >>=! { breader =>
      new fx.impl.ProxyIO with ConsoleSignature:
        override def readLine: String !@! ThisEffect = IO.blocking(breader.nn.readLine().nn)
        override def print(text: String): Unit !@! ThisEffect = IO.blocking(JConsole.out.nn.print(text))
        override def printErr(text: String): Unit !@! ThisEffect = IO.blocking(JConsole.err.nn.print(text))
        override def printLine(text: String): Unit !@! ThisEffect = IO.blocking(JConsole.out.nn.println(text))
        override def printLineErr(text: String): Unit !@! ThisEffect = IO.blocking(JConsole.err.nn.println(text))
      .toHandler
    }
