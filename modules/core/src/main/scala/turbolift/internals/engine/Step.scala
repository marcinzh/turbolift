package turbolift.internals.engine
import turbolift.!!
import turbolift.interpreter.Prompt
import StepCases._


private[engine] sealed abstract class Step(val tag: Int):
  val next: Step | Null
  final override def toString: String = Step.toStr(this)


private[engine] object StepCases:
  final class More(_tag: Int, val fun: Any => Any, override val next: Step) extends Step(_tag)

  final class Unwind(val kind: Step.UnwindKind, val prompt: Prompt | Null) extends Step(Tags.Step_Unwind):
    override val next: Null = null
    def isPop = kind == Step.UnwindKind.Pop
    def isBridge = kind == Step.UnwindKind.Bridge

  export Step.Pop


private[engine] object Step:
  val Pop: Step = new StepCases.Unwind(UnwindKind.Pop, null)
  val Cancel: Step = new StepCases.Unwind(UnwindKind.Cancel, null)
  val Throw: Step = new StepCases.Unwind(UnwindKind.Throw, null)
  val Bridge: Step = new StepCases.Unwind(UnwindKind.Bridge, null)
  def abort(prompt: Prompt): Step = new StepCases.Unwind(UnwindKind.Abort, prompt)

  enum UnwindKind:
    case Pop
    case Abort
    case Cancel
    case Throw
    case Bridge

  def toStr(step: Step): String =
    def loop(todo: Step, acc: Vector[String]): Vector[String] = 
      todo match
        case x: More => loop(x.next, acc :+ s">${##.toHexString.takeRight(4)}")
        case x: Unwind => x.kind.match
          case UnwindKind.Abort => acc :+ s"Abort(${x.prompt})"
          case k => acc :+ k.toString
    loop(step, Vector()).mkString("{", ";", "}")
