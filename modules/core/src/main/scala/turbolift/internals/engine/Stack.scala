package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.Signature
import turbolift.interpreter.{Features, Interpreter}


private abstract class Stack:
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


  final def hasSamePromptsAs(that: Stack): Boolean =
    if this eq that then
      true
    else
      deconsAndThen: (head1, tail1, _) =>
        that.deconsAndThen: (head2, tail2, _) =>
          head1.hasSamePromptsAsSeg(head2) && {
            if (tail1 == null) && (tail2 == null) then
              true
            else
              if (tail1 != null) && (tail2 != null) then
                tail1.hasSamePromptsAs(tail2)
              else
                false
          }


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


  final def locateSignature(sig: Signature, cache: Cache): Unit =
    @tailrec def loop(todo: Stack, depth: Int): Unit =
      todo.deconsAndThen: (head, tail, _) =>
        val i = head.signatures.lastIndexOf(sig)
        if i >= 0 then
          val location = head.locations(i)
          val prompt = head.prompts(location.promptIndex)
          cache.interpreter = prompt.interpreter
          cache.prompt = prompt //@#@TODO obsolete
          cache.location = location.withDepth(depth)
        else
          if tail != null then
            loop(tail, depth + 1)
          else
            sigNotFound(sig)
    loop(this, 0)


  final def locatePrompt(interp: Interpreter.Untyped, cache: Cache): Unit =
    locateSignature(interp.signatures.head, cache)


  final def locateIO: Location.Deep =
    val sig = Prompt.io.signatures.head
    @tailrec def loop(todo: Stack, depth: Int): Location.Deep =
      todo.deconsAndThen: (head, tail, _) =>
        val i = head.signatures.lastIndexOf(sig)
        if i >= 0 then
          head.locations(i).withDepth(depth)
        else
          if tail != null then
            loop(tail, depth + 1)
          else
            sigNotFound(sig)
    loop(this, 0)


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


private object Stack:
  def initial: Stack = StackSegment.initial.asStack
