package turbolift.mode
import turbolift.internals.launcher.Launcher


/** Import this, to `run` computations in multi-threaded mode (default). */
given MT: Launcher = Launcher.MT

/** Import this, to `run` computations in single-threaded mode. */
given ST: Launcher = Launcher.ST
