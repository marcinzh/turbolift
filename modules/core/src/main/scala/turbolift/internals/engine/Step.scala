package turbolift.internals.engine
import turbolift.!!
import turbolift.{ComputationCases => CC}
import turbolift.interpreter.Prompt
import turbolift.internals.engine.Misc.AnyComp


private[engine] sealed abstract class Step(val tag: Tag):
  final override def toString: String = Step.toStr(this)

  final inline def pushFlat(fun: ? => ? !! ?): Step = new Step.MoreFlat(fun.asInstanceOf[Any => AnyComp], this)
  final inline def pushPure(fun: ? => ?): Step = new Step.MorePure(fun.asInstanceOf[Any => Any], this)

  final def append(that: Step): Step =
    that match
      case Step.Pop => this
      case _ => appendLoop(that)

  private final def appendLoop(that: Step): Step =
    this match
      case thiz: Step.MoreFlat => new Step.MoreFlat(thiz.fun, thiz.next.appendLoop(that))
      case thiz: Step.MorePure => new Step.MorePure(thiz.fun, thiz.next.appendLoop(that))
      case Step.Pop => that


//@#@ public bcoz inline problem
/*private[engine]*/ object Step:
  private[engine] final class MoreFlat(val fun: Any => AnyComp, val next: Step) extends Step(Tag.MoreFlat)

  private[engine] final class MorePure(val fun: Any => Any, val next: Step) extends Step(Tag.MorePure)

  private[engine] case object Pop extends Step(Tag.Unwind)


  private[engine] def toStr(step: Step): String =
    def loop(todo: Step, acc: Vector[String]): Vector[String] = 
      todo match
        case x: MoreFlat => loop(x.next, acc :+ s">>${todo.##.toHexString.takeRight(4)}")
        case x: MorePure => loop(x.next, acc :+ s">${todo.##.toHexString.takeRight(4)}")
        case Pop => Vector("Pop")
    loop(step, Vector()).mkString("{", ";", "}")
