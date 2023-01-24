package turbolift.effects.default_handlers
import scala.util.{Random => ScalaRandom}
import turbolift.!!
import turbolift.io.IO
import turbolift.effects.{RandomEffect, RandomSig}


extension (fx: RandomEffect)
  private[effects] def randomHandler_local(seed: Long): fx.ThisHandler.FreeId =
    //@#@TODO
    ???

  private[effects] def randomHandler_local: fx.ThisHandler.Id[IO] =
    IO(ScalaRandom.nextLong) >>=! (randomHandler_local(_))
