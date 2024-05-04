package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Interpreter


private[turbolift] final class Prompt(val interpreter: Interpreter.Untyped):
  val features = interpreter.features
  val localCount: Byte = if features.isStateful then 1 else 0
  val unwind: Step = StepCases.Unwind(Step.UnwindKind.Abort, this)
  
  //@#@TODO
  private var lazyWarp: WarpImpl = null.asInstanceOf[WarpImpl]


  def isIo = features.isIo
  def signatures = interpreter.signatures
  def isStateful = features.isStateful
  def isStateless = features.isStateless
  def isParallel = features.isParallel
  def hasRestart = features.hasRestart
  def hasZip = features.hasZip
  def hasForkJoin = features.hasForkJoin

  override def toString = signatures.mkString("&")


object Prompt:
  val io: Prompt = new Prompt(Interpreter.Io.untyped)
