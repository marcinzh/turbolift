package turbolift.internals.engine
import turbolift.!!
import turbolift.internals.primitives.Tags
import StepCases._


private[engine] sealed abstract class Step(val tag: Byte):
  def doneOnce(that: Step): Step = if tag == Tags.Step_Done then that else this
  def isGlobalAbort: Boolean =
    this match
      case x: Abort => x.prompt == Prompt.global
      case _ => false

  override def toString: String =
    def loop(todo: Step, acc: Vector[String]): Vector[String] = 
      todo match
        case x: More => loop(x.next, acc :+ "More")
        case x: Hop => loop(x.next, acc :+ "Hop")
        case x: ZipLeft => loop(x.next, acc :+ "ZipLeft")
        case x: ZipRight => loop(x.next, acc :+ "ZipRight")
        case x: Restore =>  loop(x.next, acc :+ s"Rest(aside=${x.aside}, top=${x.kont.step})")
        case x: Capture => loop(x.next, acc :+ s"Cap(@${x.prompt.toStr}, aside=${x.aside})")
        case x: Abort => acc :+ s"Abort(@${x.prompt.toStr})"
        case Done => acc :+ "Done"
    loop(this, Vector()).mkString("{", "; ", "}")

private[engine] object StepCases:
  final class More(_tag: Byte, val fun: Any => Any, val next: Step) extends Step(_tag)
  final class Hop(val savedLookup: Lookup, val next: Step) extends Step(Tags.Step_Hop)
  //@#@TODO
  final class ZipLeft(val todoRight: AnyComp, fun: (Any, Any) => Any, val next: Step) extends Step(Tags.Step_ZipLeft)
  final class ZipRight(val doneLeft: Any, fun: (Any, Any) => Any, val next: Step) extends Step(Tags.Step_ZipRight)
  final class Restore(val aside: Step, val kont: Kont, val next: Step) extends Step(Tags.Step_Restore)
  final class Capture(val prompt: Prompt, val aside: Step, val next: Step) extends Step(Tags.Step_Capture)
  final class Abort(val prompt: Prompt) extends Step(Tags.Step_Abort)
  case object Done extends Step(Tags.Step_Done)

