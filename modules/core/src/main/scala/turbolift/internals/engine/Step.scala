package turbolift.internals.engine
import turbolift.!!
import turbolift.interpreter.Prompt
import turbolift.{ComputationCases => CC}
import Misc.AnyComp


private[engine] sealed abstract class Step(val tag: Tag):
  final override def toString: String = Step.toStr(this)

  final inline def pushFlat(fun: ? => ? !! ?): Step = new Step.MoreFlat(fun.asInstanceOf[Any => AnyComp], this)
  final inline def pushPure(fun: ? => ?): Step = new Step.MorePure(fun.asInstanceOf[Any => Any], this)


//@#@ public bcoz inline problem
/*private[engine]*/ object Step:
  private[engine] final class MoreFlat(val fun: Any => AnyComp, val next: Step) extends Step(Tag.MoreFlat)

  private[engine] final class MorePure(val fun: Any => Any, val next: Step) extends Step(Tag.MorePure)

  private[engine] final class Unwind(val kind: Step.UnwindKind, val prompt: Prompt | Null) extends Step(Tag.Unwind):
    def isPop = kind == Step.UnwindKind.Pop

  private[engine] val Pop: Step = new Unwind(UnwindKind.Pop, null)
  private[engine] val Cancel: Step = new Unwind(UnwindKind.Cancel, null)
  private[engine] val Throw: Step = new Unwind(UnwindKind.Throw, null)
  private[engine] def abort(prompt: Prompt): Step = new Unwind(UnwindKind.Abort, prompt)

  private[engine] enum UnwindKind:
    case Pop
    case Abort
    case Cancel
    case Throw

  private[engine] def toStr(step: Step): String =
    def loop(todo: Step, acc: Vector[String]): Vector[String] = 
      todo match
        case x: MoreFlat => loop(x.next, acc :+ s">>${todo.##.toHexString.takeRight(4)}")
        case x: MorePure => loop(x.next, acc :+ s">${todo.##.toHexString.takeRight(4)}")
        case x: Unwind => x.kind.match
          case UnwindKind.Abort => acc :+ s"Abort(${x.prompt})"
          case k => acc :+ k.toString
    loop(step, Vector()).mkString("{", ";", "}")
