package turbolift.io
import turbolift.{!!, Handler}
import turbolift.effects.IO
import turbolift.internals.primitives.{ComputationCases => CC}
import turbolift.internals.engine.WarpImpl


/** Collection of fibers (and/or other warps). Ensures lifetime boundary of its children. */

sealed trait Warp:
  def name: String
  def parent: Warp
  def isRoot: Boolean

  /** Snapshot of this warp's elements. */
  final def children: Iterable[Fiber.Untyped | Warp] !! IO = IO(unsafeChildren())

  /** Like [[children]], but filtered to return fibers only. */
  final def fibers: Iterable[Fiber.Untyped] !! IO = IO(unsafeFibers())

  /** Like [[children]], but filtered to return warps only. */
  final def warps: Iterable[Warp] !! IO = IO(unsafeWarps())

  /** Snapshot of this warp's [[Status]]. */
  final def status: Warp.Status !! IO = IO(unsafeStatus())

  /** Complete this warp, by cancelling all its children and waiting for their completion. */
  final def cancel: Unit !! IO = CC.AwaitWarp(this, isCancel = true)

  /** Like [[cancel]], but without waiting for the completion. */
  final def cancelAndForget: Unit !! IO = IO(unsafeCancelAndForget())

  /** Complete this warp, by waiting until it becomes childless. */
  final def shutdown: Unit !! IO = CC.AwaitWarp(this, isCancel = false)

  /** Like [[shutdown]], but without waiting for the completion. */
  final def shutdownAndForget: Unit !! IO = IO(unsafeShutdownAndForget())

  /** Create a child [[Fiber]]. */
  final def fork[A, U](comp: A !! U): Fiber[A, U] !! IO = CC.ForkFiber(this, comp, "")

  /** Create a child [[Warp]]. */
  final def spawn: Warp !! IO = IO(unsafeSpawn())

  /** Syntax for creating new child [[Fiber]] or [[Warp]] with a name. */
  final def named(name: String): Warp.NamedSyntax = new Warp.NamedSyntax(this, name)

  def unsafeStatus(): Warp.Status
  def unsafeChildren(): Iterable[Fiber.Untyped | Warp]
  def unsafeFibers(): Iterable[Fiber.Untyped]
  def unsafeWarps(): Iterable[Warp]
  def unsafeSpawn(name: String = ""): Warp
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

  /** The global warp.
    *
    * The parent of every initial warp.
    * There can be multiple initial warps (each with corresponding initial fiber),
    * due to reentrancy of `.run`.
    */
  def root: Warp = WarpImpl.root

  /** The outermost scoped warp. */
  def initial: Warp !! IO = CC.EnvAsk(_.initialWarp)

  /** The innermost scoped warp. */
  def current: Warp !! IO = CC.EnvAsk(_.currentWarp)

  /** Creates a new scoped warp, encompassing given computation. */
  def apply[A, U <: IO](body: A !! (U & Warp)): A !! U = CC.SpawnWarp(ExitMode.Cancel, body, "")

  /** Like [[apply]], but also passes newly created warp to given computation. */
  def use[A, U <: IO](fun: Warp => A !! (U & Warp)): A !! U = apply(current.flatMap(fun))

  /** Creates an unscoped warp. */
  def spawn: Warp !! (IO & Warp) = current.map(_.unsafeSpawn())

  /** Syntax for creating new child warp with a name. */
  def named(name: String): NamedCompanionSyntax = new NamedCompanionSyntax(name)

  final class NamedCompanionSyntax(name: String):
    def apply[A, U <: IO](body: A !! (U & Warp)): A !! U = CC.SpawnWarp(ExitMode.Cancel, body, name)
    def use[A, U <: IO](fun: Warp => A !! U): A !! U = apply(current.flatMap(fun))
    def spawn: Warp !! (IO & Warp) = current.map(_.unsafeSpawn(name))

  final class NamedSyntax(warp: Warp, name: String):
    def fork[A, U](comp: A !! U): Fiber[A, U] !! IO = CC.ForkFiber(warp, comp, name)
    def spawn: Warp !! IO = IO(warp.unsafeSpawn(name))
