package turbolift.internals.engine.stacked
import turbolift.interpreter.Void
import turbolift.internals.engine.Env


private[engine] opaque type Local = Any


private[engine] object Local:
  def void: Local = Void
  def nul: Local = null

  extension (thiz: Local)
    inline def unwrap: Any = thiz

    def isVoid: Boolean = Void == thiz
    def nonVoid: Boolean = Void != thiz
    def asEnv: Env = thiz.asInstanceOf[Env]

  object Syntax:
    extension (thiz: Any)
      inline def asLocal: Local = thiz
