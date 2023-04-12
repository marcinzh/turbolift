package turbolift.io
import turbolift.!!
import turbolift.internals.engine.FiberImpl


sealed trait Fiber[+A, -U]:
  private[turbolift] final def cast[A2, U2]: Fiber[A2, U2] = asInstanceOf[Fiber[A2, U2]]
  private[turbolift] final def impl: FiberImpl = asInstanceOf[FiberImpl]

  //@#@TODO


object Fiber:
  type Opaque = Fiber[Any, Nothing]
  private[turbolift] type Untyped = Fiber[Any, Any]
  private[turbolift] trait Unsealed extends Fiber[Any, Any]
