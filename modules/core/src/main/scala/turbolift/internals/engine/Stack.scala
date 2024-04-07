package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.Signature
import turbolift.interpreter.Features


private[engine] abstract class Stack:
  val head: StackSegment
  val features: Features


  inline final def deconsAndThen[T](inline cb: (StackSegment, Stack | Null, Step | Null) => T): T =
    this match
      case seg: StackSegment => cb(seg, null, null)
      case nel: StackNel => cb(nel.head, nel.tail, nel.aside)


  final def canPop: Boolean =
    this match
      case seg: StackSegment => head.frameCount > 1
      case nel: StackNel => true


  final def makeFork: Stack =
    this match
      case seg: StackSegment => seg.fork.asStack
      case nel: StackNel => StackNel(nel.head.fork, nel.tail.makeFork, StepCases.Pop)


  @tailrec final def containsSignature(sig: Signature): Boolean =
    deconsAndThen: (head, tail, _) =>
      val i = head.signatures.indexOf(sig)
      if i < 0 then
        if tail == null then
          false
        else
          tail.containsSignature(sig)
      else
        true


  final def locateSignature(sig: Signature): (Location.Deep, Prompt) =
    @tailrec def loop(todo: Stack, depth: Int): (Location.Deep, Prompt) =
      todo.deconsAndThen: (head, tail, _) =>
        val i = head.signatures.lastIndexOf(sig)
        if i >= 0 then
          val location = head.locations(i)
          val prompt = head.prompts(location.promptIndex)
          (location.withDepth(depth), prompt)
        else
          if tail != null then
            loop(tail, depth + 1)
          else
            sigNotFound(sig)
    loop(this, 0)

  //@#@TODO seems like silent failure to inline causes SO, despite `tailrec` passing
  inline final def locateSignatureAndThen[T](sig: Signature)(inline cb: (Location.Deep, Prompt) => T): T =
    @tailrec def loop(todo: Stack, depth: Int): T =
      todo.deconsAndThen: (head, tail, _) =>
        val i = head.signatures.lastIndexOf(sig)
        if i >= 0 then
          val location = head.locations(i)
          val prompt = head.prompts(location.promptIndex)
          cb(location.withDepth(depth), prompt)
        else
          if tail != null then
            loop(tail, depth + 1)
          else
            sigNotFound(sig)
    loop(this, 0)


  final def locatePrompt(prompt: Prompt): Location.Deep =
    locateSignatureAndThen(prompt.signatures.head): (loc, _) =>
      loc


  final def getStepAt(loc: Location.Deep): Step =
    @tailrec def loop(todo: Stack, depth: Int): Step =
      todo.deconsAndThen: (head, tail, stepAside) =>
        if depth == 0 then
          val stepTop = head.piles(loc.promptIndex).topFrame.step
          if stepTop ne StepCases.Bridge then
            stepTop
          else
            stepAside.nn
        else
          loop(tail.nn, depth - 1)
    loop(this, loc.segmentDepth)


  private final def sigNotFound(s: Signature): Nothing = panic(s"Signature ${s} not found")


  final def toStr: String = s"Stack(${toStrAux})"


  final def toStrAux: String =
    import turbolift.internals.primitives.Tags
    if this == null.asInstanceOf[Stack] then
      "NULL"
    else
      this.deconsAndThen: (head, tail, step) =>
        val a = head.toString
        if tail == null then
          a
        else
          val b = step.nn.toString
          val c = tail.nn.toStrAux
          s"$a |$b| $c"


private[engine] object Stack:
  def initial: Stack = StackSegment.initial.asStack
