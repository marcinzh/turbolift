package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.ReentrantLockImpl
import ReentrantLock.Status


sealed trait ReentrantLock:
  final def acquire: Unit !! IO = CC.intrinsic(_.intrinsicAcquireReentrantLock(this))

  final def tryAcquire: Boolean !! IO = Fiber.current.map(unsafeTryAcquire)

  final def release: Unit !! IO = !!.impure(unsafeRelease())

  final def use[A, U <: IO](body: A !! U): A !! U = IO.bracket(acquire, _ => release)(_ => body)

  final def tryUse[A, U <: IO](body: A !! U): Option[A] !! U =
    IO.bracket(tryAcquire, ok => !!.when(ok)(release))(ok => if ok then body.map(Some(_)) else !!.none)

  final def status: Status !! IO = !!.impure(unsafeStatus())


  final def owner: Option[Fiber.Untyped] !! IO =
    !!.impure:
      unsafeStatus() match
        case Status.Locked(fiber, _) => Some(fiber)
        case Status.Unlocked => None


  final def isHeldByCurrentFiber: Boolean !! IO =
    Fiber.current.map: fiber1 =>
      unsafeStatus() match
        case Status.Locked(fiber2, _) => fiber1 == fiber2
        case Status.Unlocked => false


  final def holdCount: Int !! IO =
    Fiber.current.map: fiber1 =>
      unsafeStatus() match
        case Status.Locked(fiber2, n) if fiber1 == fiber2 => n
        case _ => 0


  def unsafeTryAcquire(owner: Fiber.Untyped): Boolean
  def unsafeRelease(): Unit
  def unsafeStatus(): Status

  private[turbolift] final def asImpl: ReentrantLockImpl = asInstanceOf[ReentrantLockImpl]


object ReentrantLock:
  private[turbolift] trait Unsealed extends ReentrantLock

  enum Status:
    case Locked(owner: Fiber.Untyped, holdCount: Int)
    case Unlocked

  def create: ReentrantLock !! IO = !!.impure(unsafeCreate())
  def unsafeCreate(): ReentrantLock = new ReentrantLockImpl
