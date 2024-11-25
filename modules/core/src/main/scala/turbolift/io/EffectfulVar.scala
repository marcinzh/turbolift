package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.{IO, Broken}
import turbolift.internals.engine.concurrent.util.EffectfulVarImpl


/** Variable writable once.
  *
  * Similar to [[OnceVar]]. Starts as empty. Any `get` operation blocks until `put` happens.
  *
  * Unlike [[OnceVar]], [[EffectfulVar]] stores **effectful** value: see [[Zipper]].
  *
  * Stored effects `U` are absorbed by **the first** fiber that performs `get` operation.
  * Subsequent fibers that perform `get`s only receive pure `A` value, or `Broken` effect if none exists.
  */

sealed trait EffectfulVar[A, U <: IO] extends EffectfulVar.Get[A, U] with EffectfulVar.Put[A, U]:
  final def asGet: EffectfulVar.Get[A, U] = this
  final def asPut: EffectfulVar.Put[A, U] = this


object EffectfulVar:
  type Untyped = EffectfulVar[Any, Nothing]
  private[turbolift] trait Unsealed extends EffectfulVar[Any, Nothing]


  sealed trait Get[A, U <: IO]:
    final def get: A !! (U & Broken) =
      getOption.flatMap:
        case Some(a) => !!.pure(a)
        case None => Broken.empty

    final def getOption: Option[A] !! U = CC.intrinsic(_.intrinsicAwaitEffectfulVar(this))

    final def getOrElse(e: => Nothing): A !! U = getOption.map(_.getOrElse(e))

    final def getOrCancel: A !! U = get.handleWith(Broken.handlers.orCancel)
      getOption.flatMap:
        case Some(a) => !!.pure(a)
        case None => Broken.empty

    private[turbolift] def asImpl: EffectfulVarImpl = asInstanceOf[EffectfulVarImpl]


  sealed trait Put[A, U <: IO]:
    final def put(zipper: Zipper[A, U]): Unit !! IO = !!.impure { val _ = unsafeTryPut(zipper) }
    final def tryPut(zipper: Zipper[A, U]): Boolean !! IO = !!.impure(unsafeTryPut(zipper))
    def unsafeTryPut(zipper: Zipper[A, U]): Boolean


  def apply[A, U <: IO]: EffectfulVar[A, U] !! IO = create[A, U]
  def create[A, U <: IO]: EffectfulVar[A, U] !! IO = !!.impure(unsafeCreate())
  def unsafeCreate[A, U <: IO](): EffectfulVar[A, U] = (new EffectfulVarImpl).asInstanceOf[EffectfulVar[A, U]]

  def memoize[A, U <: IO](comp: A !! U): EffectfulVar[A, U] !! (U & Warp) =
    create[A, U].flatMap: evar =>
      Fiber.forkWithCallback(comp, evar.unsafeTryPut(_)).as(evar)
