package examples
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.effects.State


case object HandlerShadowing extends Example:
  override def description: String = s"""
    The feature required for running this example is currently disabled,
    because it turned out to be conflicting with new ones, which were more important.
  """
  // override def description: String = s"""
  //   Handler can eliminate an effect, and at the same time re-introduce it, so that
  //   it can be handled again by a different handler.
  //   This allows chaining handlers for given effect, in a pipeline-like fashion.
  //   Hypothetical application: middleware?
  // """

  val lorem = """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
  """

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
      new fx.impl.Proxy[Any] with KonsoleSignature:
        override def log(text: String): Unit !@! ThisEffect =
          !!.impure(println(text))
      .toHandler


    def sarcastic =
      new fx.impl.Proxy[Konsole] with KonsoleSignature:
        override def log(text: String): Unit !@! ThisEffect =
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
      new fx.impl.Proxy[S.type & Konsole] with KonsoleSignature:
        override def log(text: String): Unit !@! ThisEffect =
          for
            i <- S.getModify(n => (n + 1) % palette.size)
            color = palette(i)
            _ <- Konsole.log(s"${color}$text${Console.RESET}")
          yield ()
      .toHandler
      .partiallyProvideWith[Konsole](S.handler(0).dropState)

  //===== Run =====

  override def apply() =
    val lines = Utils.paragraph(lorem, 40)
    
    val prog = lines.foreachEff(Konsole.log)

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
