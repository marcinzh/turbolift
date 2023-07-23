package turbolift.concurrent
import turbolift.!!
import turbolift.internals.engine.WarpImpl


sealed trait Warp[-U]:
  private[turbolift] final def cast[U2]: Warp[U2] = asInstanceOf[Warp[U2]]
  private[turbolift] final def impl: WarpImpl = asInstanceOf[WarpImpl]

  //@#@TODO


object Warp:
  type Opaque = Warp[Nothing]
  private[turbolift] type Untyped = Warp[Any]
  private[turbolift] trait Unsealed extends Warp[Any]
