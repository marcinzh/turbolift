package turbolift.io
import turbolift.{!!, Handler}
import turbolift.effects.IO
import turbolift.internals.primitives.{ComputationCases => CC}
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
  final def cancel: Unit !! IO = CC.AwaitWarp(this, isCancel = true)

  /** Like [[cancel]], but without waiting for the completion. */
  final def cancelAndForget: Unit !! IO = IO(unsafeCancelAndForget())

  /** Complete this warp, by waiting until it becomes childless. */
  final def shutdown: Unit !! IO = CC.AwaitWarp(this, isCancel = false)

  /** Trigger completion of this warp as soon it becomes childless.
    *
    * Like [[shutdown]], but without waiting for the completion.
    */
  final def shutdownAndForget: Unit !! IO = IO(unsafeShutdownAndForget())

  /** Create a child [[Fiber]]. */
  final def fork[A, U](comp: A !! U): Fiber[A, U] !! IO = fork(name)(comp)

  /** Create a child [[Fiber]]. */
  final def fork[A, U](name: String)(comp: A !! U): Fiber[A, U] !! IO = CC.ForkFiber(this, comp, name)

  /** Create a child [[Warp]]. */
  final def spawn: Warp !! IO = spawn("")

  /** Create a child [[Warp]]. */
  final def spawn(name: String): Warp !! IO = IO(unsafeSpawn(name))

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
    case Shutdown

  /** Scoped Warp constructor lifted to a [[turbolift.Handler Handler]] value. */
  def handler(exitMode: ExitMode, name: String = ""): Handler[Identity, Identity, Warp, IO] =
    Handler.fromFunction:
      [A, U] => (comp: A !! (U & Warp)) => Warp.scoped(exitMode, name)(comp)

  /** Scoped Warp constructors lifted to [[turbolift.Handler Handler]] values. */
  object handlers:
    def cancelOnExit: Handler[Identity, Identity, Warp, IO] = handler(ExitMode.Cancel)
    def shutdownOnExit: Handler[Identity, Identity, Warp, IO] = handler(ExitMode.Shutdown)
    def cancelOnExit(name: String): Handler[Identity, Identity, Warp, IO] = handler(ExitMode.Cancel, name)
    def shutdownOnExit(name: String): Handler[Identity, Identity, Warp, IO] = handler(ExitMode.Shutdown, name)

  /** The global warp. */
  def root: Warp = WarpImpl.root

  /** The innermost scoped warp. */
  def current: Warp !! (IO & Warp) = CC.EnvAsk(_.currentWarp.nn)

  /** Creates a new unscoped warp. */
  def unscoped: Warp !! (IO & Warp) = unscoped("")

  /** Creates a new unscoped warp. */
  def unscoped(name: String): Warp !! (IO & Warp) = current.flatMap(_.spawn(name))

  /** Creates a new scoped warp. */
  def scoped[A, U <: IO](exitMode: ExitMode, name: String = "")(body: A !! (U & Warp)): A !! U = CC.SpawnWarp(exitMode, body, name)

  /** Creates a new scoped warp. */
  def cancelOnExit[A, U <: IO](body: A !! (U & Warp)): A !! U = cancelOnExit("")(body)

  /** Creates a new scoped warp. */
  def cancelOnExit[A, U <: IO](name: String)(body: A !! (U & Warp)): A !! U = scoped(ExitMode.Cancel, name)(body)

  /** Creates a new scoped warp. */
  def shutdownOnExit[A, U <: IO](body: A !! (U & Warp)): A !! U = shutdownOnExit("")(body)

  /** Creates a new scoped warp. */
  def shutdownOnExit[A, U <: IO](name: String)(body: A !! (U & Warp)): A !! U = scoped(ExitMode.Shutdown, name)(body)
