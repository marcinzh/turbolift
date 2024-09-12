package turbolift.internals.engine
import turbolift.!!
import turbolift.interpreter.Prompt
import StepCases._


private[engine] sealed abstract class Step(val tag: Int):
  val next: Step | Null
  final override def toString: String = Step.toStr(this)


private[engine] object StepCases:
  final class More(_tag: Int, val fun: Any => Any, override val next: Step) extends Step(_tag)
  sealed abstract class HasNoNext(_tag: Int) extends Step(_tag) { override val next: Null = null }
  final class Unwind(val kind: Step.UnwindKind, val prompt: Prompt | Null) extends HasNoNext(Tags.Step_Unwind)
  case object Bridge extends HasNoNext(Tags.Step_Bridge)

  export Step.Pop


private[engine] object Step:
  val Pop = new StepCases.Unwind(UnwindKind.Pop, null)
  val Cancel = new StepCases.Unwind(UnwindKind.Cancel, null)
  val Throw = new StepCases.Unwind(UnwindKind.Throw, null)

  def abort(prompt: Prompt): Step = new StepCases.Unwind(UnwindKind.Abort, prompt)

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
        case x: Unwind => x.kind.match
          case UnwindKind.Abort => acc :+ s"Abort(${x.prompt})"
          case k => acc :+ k.toString
        case Bridge => acc :+ "Bridge"
    loop(step, Vector()).mkString("{", ";", "}")
