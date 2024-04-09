package turbolift.internals.engine
import turbolift.!!
import turbolift.internals.primitives.Tags
import StepCases._


private[engine] sealed abstract class Step(val tag: Byte):
  val next: Step | Null
  final override def toString: String = Step.toStr(this)


private[engine] object StepCases:
  final class More(_tag: Byte, val fun: Any => Any, override val next: Step) extends Step(_tag)
  final class ZipSeqLeft(val todoRight: () => AnyComp, val fun: (Any, Any) => Any, override val next: Step) extends Step(Tags.Step_ZipSeqLeft)
  final class ZipSeqRight(val doneLeft: Any, val fun: (Any, Any) => Any, override val next: Step) extends Step(Tags.Step_ZipSeqRight)
  final class Capture(val prompt: Prompt, val aside: Step, override val next: Step) extends Step(Tags.Step_Capture)
  final class Push(val body: AnyComp, val prompt: Prompt, override val next: Step) extends Step(Tags.Step_Push)
  sealed abstract class HasNoNext(_tag: Byte) extends Step(_tag) { override val next: Null = null }
  final class Unwind(val kind: Step.UnwindKind, val prompt: Prompt | Null) extends HasNoNext(Tags.Step_Unwind)
  case object Bridge extends HasNoNext(Tags.Step_Bridge)
  export Step.Pop


private[engine] object Step:
  val Pop = new StepCases.Unwind(UnwindKind.Pop, null)
  val Cancel = new StepCases.Unwind(UnwindKind.Cancel, null)
  val Throw = new StepCases.Unwind(UnwindKind.Throw, null)

  enum UnwindKind:
    def isPop = this == UnwindKind.Pop
    case Pop
    case Abort
    case Cancel
    case Throw

  def toStr(step: Step): String =
    def loop(todo: Step, acc: Vector[String]): Vector[String] = 
      todo match
        case x: More => loop(x.next, acc :+ s">${##.toHexString.takeRight(4)}")
        case x: ZipSeqLeft => loop(x.next, acc :+ "ZipSeqLeft")
        case x: ZipSeqRight => loop(x.next, acc :+ "ZipSeqRight")
        case x: Capture => loop(x.next, acc :+ s"Cap(@${x.prompt} aside=${toStr(x.aside)})")
        case x: Push => loop(x.next, acc :+ s"Push(${x.prompt})")
        case x: Unwind => x.kind.match
          case UnwindKind.Abort => acc :+ s"Abort(${x.prompt})"
          case k => acc :+ k.toString
        case Bridge => acc :+ "Bridge"
    loop(step, Vector()).mkString("{", ";", "}")
