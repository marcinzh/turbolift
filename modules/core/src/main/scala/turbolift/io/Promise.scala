package turbolift.io
import turbolift.!!


sealed abstract class Promise[+A, -U]:
  ;
  //@#@TODO


object Promise:
  private[turbolift] type Untyped = Promise[Any, Any]
  private[turbolift] trait Unsealed extends Promise[Any, Any]
