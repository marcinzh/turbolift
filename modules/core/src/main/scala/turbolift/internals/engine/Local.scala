package turbolift.internals.engine
import turbolift.interpreter.Void


private[engine] opaque type Local = Any


private[engine] object Local:
  def void: Local = Void
  def nul: Local = null

  extension (thiz: Local)
    inline def unwrap: Any = thiz

    def isVoid: Boolean = Void == thiz
    def nonVoid: Boolean = Void != thiz
    def asEnv: Env = thiz.asInstanceOf[Env]


extension (thiz: Any)
  private[engine] inline def asLocal: Local = thiz
