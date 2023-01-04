package turbolift.internals.engine
import turbolift.internals.interpreter.Void
import turbolift.internals.primitives.Tags


private[engine] final class Segment(
  val frame: Frame,
  val savedStan: Any,
  val step: Step,
  val history: History,
):
  def copy(
    frame: Frame = frame,
    savedStan: Any = savedStan,
    step: Step = step,
    history: History = history,
  ): Segment = new Segment(
    frame = frame,
    savedStan = savedStan,
    step = step,
    history = history,
  )

  def height: Int = frame.height

  def isBase: Boolean = frame.isBase

  override def toString =
    val s = if savedStan == Void then "" else s"=${savedStan}"
    val h = frame.height
    val b = if isBase then "!" else ""
    s"^${h}${s}$b"

  def patch(s: Any): Segment =
    if Void == s then
      this
    else
      copy(savedStan = s)

  def unpatch: Segment =
    if Void == savedStan then
      this
    else
      copy(savedStan = Void)



private[engine] object Segment:
  extension (thiz: List[Segment])
    def patch(s: Any): List[Segment] =
      if Void == s then
        thiz
      else
        thiz match
          case Nil => Nil
          case g :: gs => g.copy(savedStan = s) :: gs

    def unpatch: List[Segment] =
      thiz match
        case Nil => Nil
        case g :: gs =>
          if Void == g.savedStan then
            thiz
          else
            g.copy(savedStan = Void) :: gs
