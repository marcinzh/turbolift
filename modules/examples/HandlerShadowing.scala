package examples
import turbolift.!!
import turbolift.Extensions._
import turbolift.io.IO
import turbolift.effects.{Console, ConsoleSignature, State}


case object HandlerShadowing extends Function0[Unit]:
  private val lorem = """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
  """

  private val lines =
    def loop(words: Vector[String], line: String, lines: Vector[String]): Vector[String] =
      words match
        case Vector() => lines :+ line
        case w +: ws =>
          val line2 = if line.isEmpty then w else s"$line $w"
          if line2.size < 40
          then loop(ws.toVector, line2, lines)
          else loop(ws.toVector, w, lines :+ line)
        case _ => ???
    loop(lorem.split("\\s+").filter(_.nonEmpty).toVector, "", Vector())

  private val palette = Vector(
    Console.MAGENTA,
    Console.RED,
    Console.YELLOW,
    Console.GREEN,
    Console.CYAN,
    Console.BLUE,
  )

  extension (fx: Console.type)
    private def rainbow: fx.ThisHandler.Id[Console & IO] =
      case object S extends State[Int]

      new fx.Proxy[S.type & Console] with ConsoleSignature:
        override def print(text: String): Unit !@! ThisEffect = _ => Console.print(text)
        override def printErr(text: String): Unit !@! ThisEffect = _ => Console.printErr(text)
        override def printLineErr(text: String): Unit !@! ThisEffect = _ => Console.printLineErr(text)
        override def readLine: String !@! ThisEffect = _ => Console.readLine

        override def printLine(text: String): Unit !@! ThisEffect =
          _ =>
            for
              i <- S.getModify(n => (n + 1) % palette.size)
              color = palette(i)
              _ <- Console.printLine(s"${color}$text${Console.RESET}")
            yield ()
      .toHandler
      .partiallyProvideWith[Console](S.handlers.shared(0).dropState)


    private def sarcastic: fx.ThisHandler.Id[Console & IO] =
      case object S extends State[Boolean]

      new fx.Proxy[S.type & Console] with ConsoleSignature:
        override def print(text: String): Unit !@! ThisEffect = _ => Console.print(text)
        override def printErr(text: String): Unit !@! ThisEffect = _ => Console.printErr(text)
        override def printLineErr(text: String): Unit !@! ThisEffect = _ => Console.printLineErr(text)
        override def readLine: String !@! ThisEffect = _ => Console.readLine

        override def printLine(text: String): Unit !@! ThisEffect =
          _ =>
            for
              x <- S.get
              (text2, x2) = text.foldLeft(("", x)) {
                case ((acc, x), char) =>
                  if char.isLetter then
                    val char2 = if x then char.toUpper else char.toLower
                    (acc :+ char2, !x)
                  else
                    (acc :+ char, x)
              }
              _ <- S.put(x2)
              _ <- Console.printLine(text2)
            yield ()
      .toHandler
      .partiallyProvideWith[Console](S.handlers.shared(true).dropState)


  override def apply() =
    val prog = lines.foreach_!!(Console.println)

    val cases = List(
      ("Plain", Console.handler),
      ("Rainbow", Console.rainbow %%! Console.handler),
      ("Sarcastic", Console.sarcastic %%! Console.handler),
      ("Sarcastic & Rainbow", Console.sarcastic %%! Console.rainbow %%! Console.handler),
    )

    for (label, handler) <- cases do
      val title = s"$label Console handler:"
      val line = "=" * title.size
      println(line)
      println(title)
      println(line)
      prog.handleWith(handler).unsafeRun
      println
