package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.MutexImpl


sealed trait Mutex:
  final def acquire: Unit !! IO = CC.intrinsic(_.intrinsicAcquireMutex(this))

  final def tryAcquire: Boolean !! IO = !!.impure(unsafeTryAcquire())

  final def release: Unit !! IO = !!.impure(unsafeRelease())

  final def use[A, U <: IO](body: A !! U): A !! U = IO.bracket(acquire, _ => release)(_ => body)

  final def tryUse[A, U <: IO](body: A !! U): Option[A] !! U =
    IO.bracket(tryAcquire, ok => !!.when(ok)(release))(ok => if ok then body.map(Some(_)) else !!.none)

  final def isLocked: Boolean !! IO = !!.impure(unsafeIsLocked())

  def unsafeTryAcquire(): Boolean
  def unsafeRelease(): Unit
  def unsafeIsLocked(): Boolean

  private[turbolift] final def asImpl: MutexImpl = asInstanceOf[MutexImpl]


object Mutex:
  private[turbolift] trait Unsealed extends Mutex

  def create: Mutex !! IO = !!.impure(unsafeCreate())
  def unsafeCreate(): Mutex = new MutexImpl
