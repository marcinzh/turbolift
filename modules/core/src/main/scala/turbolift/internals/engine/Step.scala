package turbolift.internals.engine
import turbolift.!!
import turbolift.interpreter.Prompt
import turbolift.{ComputationCases => CC}


private[engine] sealed abstract class Step(val tag: Tag):
  final override def toString: String = Step.toStr(this)
  // final inline def push(comp: CC.Map[Any, Any, Any, Any]): Step =
  //   Step.More(comp.tag + Tag.MoreFlat - Tag.FlatMap, comp, this)


private[engine] object Step:
  final class More(_tag: Tag, val fun: Any => Any, val next: Step) extends Step(_tag)

  final class Unwind(val kind: Step.UnwindKind, val prompt: Prompt | Null) extends Step(Tag.Unwind):
    def isPop = kind == Step.UnwindKind.Pop
    def isBridge = kind == Step.UnwindKind.Bridge

  val Pop: Step = new Unwind(UnwindKind.Pop, null)
  val Cancel: Step = new Unwind(UnwindKind.Cancel, null)
  val Throw: Step = new Unwind(UnwindKind.Throw, null)
  val Bridge: Step = new Unwind(UnwindKind.Bridge, null)
  def abort(prompt: Prompt): Step = new Unwind(UnwindKind.Abort, prompt)

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
