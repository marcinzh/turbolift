package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO


sealed trait Fiber[+A, -U]:
  def name: String
  def parent: Fiber.Untyped | Warp

  /** Snapshot of this fiber's [[Fiber.Status Status]]. */
  final def status: Fiber.Status !! IO = !!.impure(unsafeStatus())

  /** Awaits completion of this fiber, returns its **pure** result and absorbs its effects. */
  final def join: A !! (U & IO) = await.flatMap(_.run)

  /** Awaits completion of this fiber and returns its **effectful** result. */
  final def await: Zipper[A, U] !! IO = CC.intrinsic(_.intrinsicAwaitFiber(this, isCancel = false, isVoid = false))

  /** Awaits completion of this fiber and disards its result (including the effects). */
  final def awaitVoid: Unit !! IO = CC.intrinsic(_.intrinsicAwaitFiber(this, isCancel = false, isVoid = true))

  /** Cancels this fiber and await its completion. */
  final def cancel: Unit !! IO = CC.intrinsic(_.intrinsicAwaitFiber(this, isCancel = true, isVoid = true))

  /** Cancels this fiber without awaiting its completion. */
  final def cancelAndForget: Unit !! IO = !!.impure(unsafeCancelAndForget())

  /** Get result of this fiber now, or cancel it if it's still pending. */
  final def nowOrNever: Zipper[A, U] !! IO = CC.intrinsic(_.intrinsicAwaitFiber(this, isCancel = true, isVoid = false))

  /** Get result of this fiber now, or fail it's still pending. */
  final def getOrDie: Zipper[A, U] !! IO = poll.flatMap(IO.fromOption(_)(new Exceptions.Pending))

  /** Try to get result of this fiber now. */
  final def poll: Option[Zipper[A, U]] !! IO = !!.impure(unsafePoll())

  def unsafeCancelAndForget(): Unit
  def unsafeStatus(): Fiber.Status
  def unsafePoll(): Option[Zipper[A, U]]

  final def untyped: Fiber.Untyped = this
  final def cast[A2, U2]: Fiber[A2, U2] = asInstanceOf[Fiber[A2, U2]]


object Fiber:
  type Untyped = Fiber[Any, Nothing]
  private[turbolift] trait Unsealed extends Fiber[Any, Nothing]

  enum Status:
    case Pending(role: Role, isRacer: Boolean, isCancelled: Boolean)
    case Completed(outcome: Outcome[Unit])

  enum Role:
    case Runner
    case Standby
    case Arbiter(racers: List[Fiber.Untyped])
    case Waiter(waitee: Fiber.Untyped | Warp | OnceVar.Untyped)
    case Blocker

  /** Get the current fiber. */
  def current: Fiber.Untyped !! IO = CC.intrinsic(_.intrinsicCurrentFiber())

  /** Get the current fiber, explicitly ascribing its type. */
  def currentTypeunsafe[A, U]: Fiber[A, U] !! IO = current.cast[Fiber[A, U], IO]

  /** Create a new fiber in the current [[Warp]]. */
  def fork[A, U](comp: A !! U): Fiber[A, U] !! (U & IO & Warp) = CC.intrinsic(_.intrinsicForkFiber(null, comp, ""))

  /** Create a new fiber in specified [[Warp]]. */
  def forkAt[A, U](warp: Warp)(comp: A !! U): Fiber[A, U] !! (U & IO) = CC.intrinsic(_.intrinsicForkFiber(warp, comp, ""))

  /** Experimental */
  def forkWithCallback[A, U](
    comp: A !! U,
    callback: Zipper[A, U] => Unit,
    name: String = "",
  ): Fiber[A, U] !! (U & IO & Warp) = CC.intrinsic(_.intrinsicForkFiber(null, comp, name, callback.asInstanceOf[Zipper.Untyped => Unit]))

  /** Syntax for creating new [[Fiber]] with a name. */
  def named(name: String) = new NamedSyntax(name)

  final class NamedSyntax(name: String):
    def fork[A, U](comp: A !! U): Fiber[A, U] !! (U & IO & Warp) = CC.intrinsic(_.intrinsicForkFiber(null, comp, name))
    def forkAt[A, U](warp: Warp)(comp: A !! U): Fiber[A, U] !! (U & IO) = CC.intrinsic(_.intrinsicForkFiber(warp, comp, name))
