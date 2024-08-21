package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.OnceVarImpl


sealed trait OnceVar[A] extends OnceVar.Get[A] with OnceVar.Put[A]:
  final def asGet: OnceVar.Get[A] = this
  final def asPut: OnceVar.Put[A] = this



object OnceVar:
  type Untyped = OnceVar[Any]
  private[turbolift] trait Unsealed extends OnceVar[Any]


  sealed trait Get[A]:
    @annotation.targetName("get_OnceVar")
    final def get: A !! IO = CC.intrinsic(_.intrinsicAwaitOnceVar(this))
    final def tryGet: Option[A] !! IO = !!.impure(unsafeTryGet)
    def unsafeTryGet: Option[A]

    private[turbolift] def asImpl: OnceVarImpl = asInstanceOf[OnceVarImpl]


  sealed trait Put[A]:
    final def put(value: A): Unit !! IO = !!.impure { val _ = unsafeTryPut(value) }
    final def tryPut(value: A): Boolean !! IO = !!.impure(unsafeTryPut(value))
    def unsafeTryPut(value: A): Boolean


  def fresh[A]: OnceVar[A] !! IO = !!.impure((new OnceVarImpl).asInstanceOf[OnceVar[A]])
