package turbolift.runtime


type Mode = Option[RuntimeConfig]

/** Convenience definition used to automatically select [[RuntimeConfig]] for `.run`. */
object Mode:
  val MT = Some(RuntimeConfig.MT)
  val ST = Some(RuntimeConfig.ST)

/** Import this, to `run` computations in multi-threaded mode (default). */
given MT: Mode = Mode.MT

/** Import this, to `run` computations in single-threaded mode. */
given ST: Mode = Mode.ST
