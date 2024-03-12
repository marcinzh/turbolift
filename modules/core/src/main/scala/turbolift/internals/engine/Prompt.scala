package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Interpreter


private[turbolift] final class Prompt(val interpreter: Interpreter.Untyped):
  val features = interpreter.features
  val stanCount: Byte = if features.isStateful then 1 else 0
  val unwind: Step = StepCases.Unwind(this, false)
  
  //@#@TODO
  private var lazyWarp: WarpImpl = null.asInstanceOf[WarpImpl]


  def isRoot = features.isRoot
  def signatures = interpreter.signatures
  def isStateful = features.isStateful
  def isStateless = features.isStateless
  def isParallel = features.isParallel
  def hasRestart = features.hasRestart
  def hasZip = features.hasZip
  def hasForkJoin = features.hasForkJoin

  def asMark: Mark = Mark.wrap(this)

  override def toString = signatures.mkString("&")
