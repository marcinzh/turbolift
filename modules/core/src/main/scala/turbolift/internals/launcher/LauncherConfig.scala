package turbolift.internals.launcher


case class LauncherConfig(
   multiThreaded: Boolean,
)

object LauncherConfig:
  val default: LauncherConfig = LauncherConfigs.MT

object LauncherConfigs:
  /** Import this, to `run` computations in multi-threaded mode (default). */
  given MT: LauncherConfig = LauncherConfig(multiThreaded = true)

  /** Import this, to `run` computations in single-threaded mode. */
  given ST: LauncherConfig = LauncherConfig(multiThreaded = false)
