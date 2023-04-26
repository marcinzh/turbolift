package examples
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.effects.State


case object HandlerShadowing extends Function0[Unit]:
  val lorem = """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
  """

  val lines =
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

  val palette = Vector(
    Console.MAGENTA,
    Console.RED,
    Console.YELLOW,
    Console.GREEN,
    Console.CYAN,
    Console.BLUE,
  )

  //===== Effect =====

  trait KonsoleSignature extends Signature:
    def log(text: String): Unit !@! ThisEffect

  case object Konsole extends Effect[KonsoleSignature] with KonsoleSignature:
    override def log(text: String): Unit !@! ThisEffect = perform(_.log(text))

  type Konsole = Konsole.type

  //===== Handlers =====

  extension (fx: Konsole)
    def plain =
      new fx.Proxy[Any] with KonsoleSignature:
        override def log(text: String): Unit !@! ThisEffect =
          _ => !!.impure(println(text))
      .toHandler


    def sarcastic =
      new fx.Proxy[Konsole] with KonsoleSignature:
        override def log(text: String): Unit !@! ThisEffect =
          _ =>
            val (text2, _) = text.foldLeft(("", false)) {
              case ((acc, flip), char) =>
                if char.isLetter then
                  val char2 = if flip then char.toUpper else char.toLower
                  (acc :+ char2, !flip)
                else
                  (acc :+ char, flip)
            }
            Konsole.log(text2)
      .toHandler


    def rainbow =
      case object S extends State[Int]
      new fx.Proxy[S.type & Konsole] with KonsoleSignature:
        override def log(text: String): Unit !@! ThisEffect =
          _ =>
            for
              i <- S.getModify(n => (n + 1) % palette.size)
              color = palette(i)
              _ <- Konsole.log(s"${color}$text${Console.RESET}")
            yield ()
      .toHandler
      .partiallyProvideWith[Konsole](S.handler(0).dropState)

  //===== Run =====

  override def apply() =
    val prog = lines.foreach_!!(Konsole.log)

    val cases = List(
      ("Plain", Konsole.plain),
      ("Rainbow", Konsole.rainbow %%! Konsole.plain),
      ("Sarcastic", Konsole.sarcastic %%! Konsole.plain),
      ("Sarcastic + Rainbow", Konsole.sarcastic %%! Konsole.rainbow %%! Konsole.plain),
    )

    for (label, handler) <- cases do
      val title = s"Konsole handler = $label"
      val line = "=" * title.size
      println(line)
      println(title)
      println(line)
      prog.handleWith(handler).unsafeRun
      println
