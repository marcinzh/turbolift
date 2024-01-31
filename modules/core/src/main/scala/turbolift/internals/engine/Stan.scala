package turbolift.internals.engine
import turbolift.interpreter.Void


private[engine] opaque type Stan = Any


private[engine] object Stan:
  def void: Stan = Void
  def nul: Stan = null

  extension (thiz: Stan)
    inline def unwrap: Any = thiz

    def isVoid: Boolean = Void == thiz
    def nonVoid: Boolean = Void != thiz
    def asEnv: Env = thiz.asInstanceOf[Env]


extension (thiz: Any)
  private[engine] inline def asStan: Stan = thiz
