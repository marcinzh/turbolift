package turbolift.io
import turbolift.!!
import turbolift.effects.IO
// import Loom.Done
import Loom.Event


/** Fiber factory. Fibers share their effects, and the loom accumulates their effectful results.
 *
 * Unrelated to the "Project Loom".
 */


//// Experimental
final class Loom[A, U <: IO] private (
  private val theWarp: Warp,
  private val theChannel: Channel[Event[A, U]],
):
  def done: Unit !! IO = theChannel.put(Event.Done())

  def unsafeSubmit(comp: A !! U): Unit = theChannel.unsafeTryPut(Event.Fork(comp))

  def submit(comp: A !! U): Unit !! IO = theChannel.put(Event.Fork(comp))

  def fork(comp: A !! U): Fiber[A, U] !! IO =
    for
      ovar <- OnceVar.create[Fiber[A, U]]
      _ <- submit(Fiber.currentTypeunsafe[A, U].flatMap(ovar.put) &&! comp)
      fib <- ovar.get
    yield fib

  def fold[B](initial: B)(op: (B, A) => B): B !! U = foldEff(initial)((b, a) => !!.pure(op(b, a)))

  def foldEff[B, V <: U](initial: B)(op: (B, A) => B !! V): B !! V = foldZipper(initial)((b, zipp) => zipp.run.flatMap(op(b, _)))

  def reduceOption[B](op: (B, A) => B): Option[B] !! U = fold(None: Option[B])((mb, a) => mb.map(op(_, a)))

  def reduceOptionEff[B, V <: U](op: (B, A) => B !! V): Option[B] !! V = foldEff(None: Option[B])((mb, a) => mb.fold(!!.none)(op(_, a).map(Some(_))))

  def foldZipper[B, V <: U](initial: B)(op: (B, Zipper[A, U]) => B !! V): B !! V =
    def loop(b: B): B !! V =
      theChannel.get.flatMap: event =>
        event match
          case Event.Done() => !!.pure(b)
          case Event.Join(zipp) => op(b, zipp).flatMap(loop)
          case Event.Fork(comp) =>
            Fiber.forkWithCallbackAt(theWarp, comp, zipp => theChannel.unsafeTryPut(Event.Join(zipp)))
            .&&!(loop(b))
    loop(initial)

  def reduceZipperVoid[V <: U](op: Zipper[A, U] => Unit !! V): Unit !! V =
    foldZipper(())((_, zipp) => op(zipp))

  def reduceVoid: Unit !! U = reduceZipperVoid(_.run.void)


object Loom:
  private enum Event[A, U]:
    case Done()
    case Fork(comp: A !! U)
    case Join(zipp: Zipper[A, U])


  def create[A, U <: IO](name: String): Loom[A, U] !! (IO & Warp) = Warp.unscoped.flatMap(createAt(_, name))
  def create[A, U <: IO]: Loom[A, U] !! (IO & Warp) = Warp.unscoped.flatMap(createAt(_, ""))

  def createAt[A, U <: IO](warp: Warp, name: String = ""): Loom[A, U] !! IO = !!.impure(unsafeCreateAt(warp, name))
  def unsafeCreateAt[A, U <: IO](warp: Warp, name: String = ""): Loom[A, U] = new Loom[A, U](warp.unsafeSpawn(name), Channel.unsafeUnbounded)

