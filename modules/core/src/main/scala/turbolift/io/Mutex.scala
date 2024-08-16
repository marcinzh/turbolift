package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.MutexImpl


sealed trait Mutex:
  final def acquire: Unit !! IO = CC.intristic(_.intristicAcquireMutex(this))

  final def release: Unit !! IO = !!.impure(unsafeRelease())

  final def lock[A, U <: IO](body: A !! U): A !! U = IO.bracket(acquire)(_ => release)(_ => body)

  def unsafeRelease(): Unit

  private[turbolift] final def asImpl: MutexImpl = asInstanceOf[MutexImpl]


object Mutex:
  private[turbolift] trait Unsealed extends Mutex

  def fresh: Mutex !! IO = !!.impure(new MutexImpl)
