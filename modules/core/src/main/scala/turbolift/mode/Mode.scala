package turbolift.mode


final case class Mode(multiThreaded: Boolean)

object Mode:
  val default: Mode = MT

/** Import this, to `run` computations in multi-threaded mode (default). */
given MT: Mode = Mode(true)

/** Import this, to `run` computations in single-threaded mode. */
given ST: Mode = Mode(false)
