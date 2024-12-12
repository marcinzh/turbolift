package turbolift.io
import turbolift.{!!, Handler, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.WarpImpl
import turbolift.Extensions.Identity


/** Collection of fibers (and/or other warps). Ensures lifetime boundary of its children. */

sealed trait Warp:
  def name: String

  /** The parent of this warp.
   *
   *  - For scoped warp: the [[Fiber]] in which this warp was created.
   *  - For unscoped warp: the [[Warp]] in which this warp was created.
   *  - For the [[Warp.root root]] warp: `None`.
   */
  def parent: Option[Warp | Fiber.Untyped]

  /** For scoped warp only: the nearest scoped warp enclosing this one. */
  def outer: Option[Warp]

  /** Snapshot of this warp's elements. */
  final def children: Iterable[Fiber.Untyped | Warp] !! IO = IO(unsafeChildren())

  /** Like [[children]], but filtered to return fibers only. */
  final def fibers: Iterable[Fiber.Untyped] !! IO = IO(unsafeFibers())

  /** Like [[children]], but filtered to return warps only. */
  final def warps: Iterable[Warp] !! IO = IO(unsafeWarps())

  /** Snapshot of this warp's [[Warp.Status Status]]. */
  final def status: Warp.Status !! IO = IO(unsafeStatus())

  /** Complete this warp, by cancelling all its children and waiting for their completion. */
  final def cancel: Unit !! IO = CC.intrinsic(_.intrinsicAwaitWarp(this, isCancel = true))

  /** Like [[cancel]], but without waiting for the completion. */
  final def cancelAndForget: Unit !! IO = IO(unsafeCancelAndForget())

  /** Complete this warp, by waiting until it becomes childless. */
  final def await: Unit !! IO = CC.intrinsic(_.intrinsicAwaitWarp(this, isCancel = false))

  /** Trigger completion of this warp as soon it becomes childless.
    *
    * Like [[await]], but without waiting for the completion.
    */
  final def shutdownAndForget: Unit !! IO = IO(unsafeShutdownAndForget())

  /** Create a child [[Fiber]]. */
  final def fork[A, U](comp: A !! U): Fiber[A, U] !! (U & IO) = Fiber.forkAt(this)(comp)

  /** Create a child [[Fiber]]. */
  final def fork[A, U](name: String)(comp: A !! U): Fiber[A, U] !! (U & IO) = Fiber.named(name).forkAt(this)(comp)

  /** Create a child [[Warp]]. */
  final def spawn: Warp !! IO = spawn("")

  /** Create a child [[Warp]]. */
  final def spawn(name: String): Warp !! IO = IO(unsafeSpawn(name))

  /** Create a [[Loom]] with this warp as a parent. */
  final def loom[A, U <: IO]: Loom[A, U] !! IO = loom("")

  /** Create a [[Loom]] with this warp as a parent. */
  final def loom[A, U <: IO](name: String): Loom[A, U] !! IO = Loom.createAt(this)


  def unsafeStatus(): Warp.Status
  def unsafeChildren(): Iterable[Fiber.Untyped | Warp]
  def unsafeFibers(): Iterable[Fiber.Untyped]
  def unsafeWarps(): Iterable[Warp]
  def unsafeSpawn(name: String): Warp
  def unsafeShutdownAndForget(): Unit
  def unsafeCancelAndForget(): Unit


object Warp:
  private[turbolift] trait Unsealed extends Warp

  enum Status:
    case Pending(fiberCount: Int, warpCount: Int, isShutdown: Boolean, isCancelled: Boolean)
    case Completed

  enum ExitMode:
    case Cancel
    case Await

  /** Scoped Warp constructor lifted to a [[turbolift.Handler Handler]] value. */
  def handler(exitMode: ExitMode, name: String = ""): Handler[Identity, Identity, Warp, IO] =
    Handler.fromFunction:
      [A, U] => (comp: A !! (U & Warp)) => Warp.scoped(exitMode, name)(comp)

  /** Scoped Warp constructors lifted to [[turbolift.Handler Handler]] values. */
  object handlers:
    def cancel: Handler[Identity, Identity, Warp, IO] = handler(ExitMode.Cancel)
    def await: Handler[Identity, Identity, Warp, IO] = handler(ExitMode.Await)
    def cancel(name: String): Handler[Identity, Identity, Warp, IO] = handler(ExitMode.Cancel, name)
    def await(name: String): Handler[Identity, Identity, Warp, IO] = handler(ExitMode.Await, name)

  /** The global warp. */
  def root: Warp = WarpImpl.root

  /** The innermost scoped warp. */
  def current: Warp !! (IO & Warp) = CC.intrinsic(_.intrinsicEnvAsk(_.currentWarp.nn))

  /** Creates a new unscoped warp, as a child of the current scoped warp. */
  def unscoped: Warp !! (IO & Warp) = unscoped("")

  /** Creates a new unscoped warp, as a child of the current scoped warp. */
  def unscoped(name: String): Warp !! (IO & Warp) = current.flatMap(_.spawn(name))

  /** Creates a new scoped warp. */
  def scoped[A, U <: IO](exitMode: ExitMode, name: String = "")(body: A !! (U & Warp)): A !! U = CC.intrinsic(_.intrinsicSpawnWarp(exitMode, body, name))

  /** Creates a new scoped warp that cancels its children on exit. */
  def cancelling[A, U <: IO](body: A !! (U & Warp)): A !! U = cancelling("")(body)

  /** Creates a new scoped warp that cancels its children on exit. */
  def cancelling[A, U <: IO](name: String)(body: A !! (U & Warp)): A !! U = scoped(ExitMode.Cancel, name)(body)

  /** Creates a new scoped warp that awaits its children on exit. */
  def awaiting[A, U <: IO](body: A !! (U & Warp)): A !! U = awaiting("")(body)

  /** Creates a new scoped warp that awaits its children on exit. */
  def awaiting[A, U <: IO](name: String)(body: A !! (U & Warp)): A !! U = scoped(ExitMode.Await, name)(body)

  /** Same as [[cancelling]]. */ 
  def apply[A, U <: IO](body: A !! (U & Warp)): A !! U = apply("")(body)

  /** Same as [[cancelling]]. */ 
  def apply[A, U <: IO](name: String)(body: A !! (U & Warp)): A !! U = cancelling(name)(body)
