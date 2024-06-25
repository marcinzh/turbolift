package turbolift.io
import turbolift.!!
import turbolift.effects.IO
import turbolift.internals.engine.OnceVarImpl
import turbolift.internals.primitives.{ComputationCases => CC}


sealed trait OnceVarPut[A]:
  final def put(value: A): Unit !! IO = !!.impure { val _ = unsafeTryPut(value) }
  final def tryPut(value: A): Boolean !! IO = !!.impure(unsafeTryPut(value))
  def unsafeTryPut(value: A): Boolean


sealed trait OnceVarGet[A]:
  @annotation.targetName("get_public")
  final def get: A !! IO = CC.AwaitOnceVar(this)
  final def tryGet: Option[A] !! IO = !!.impure(unsafeTryGet)
  def unsafeTryGet: Option[A]


sealed trait OnceVar[A] extends OnceVarGet[A] with OnceVarPut[A]:
  final def asGet: OnceVarGet[A] = this
  final def asPut: OnceVarPut[A] = this
  final def untyped: Fiber.Untyped = asInstanceOf[Fiber.Untyped]


object OnceVar:
  type Untyped = OnceVar[Any]
  private[turbolift] trait Unsealed extends OnceVar[Any]

  def fresh[A]: OnceVar[A] !! IO = !!.impure((new OnceVarImpl).asInstanceOf[OnceVar[A]])
