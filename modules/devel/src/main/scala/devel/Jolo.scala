package devel
import org.openjdk.jol.info.ClassLayout
import org.openjdk.jol.vm.VM


object Jolo:
  def run =
    println(VM.current.nn.details)
    """
      turbolift.internals.engine.concurrent.FiberImpl
      turbolift.internals.engine.concurrent.FiberImpl$Extra
      turbolift.internals.engine.concurrent.WarpImpl
      turbolift.internals.engine.concurrent.util.OnceVarImpl
      turbolift.internals.engine.concurrent.util.EffectfulVarImpl
      turbolift.internals.engine.stacked.Stack
      turbolift.internals.engine.Engine
      turbolift.internals.executor.ReentrantExecutor
    """
    .split("\n").map(_.trim).filter(_.nonEmpty)
    .foreach(show)


  def show(s: String) =
    try
      val klass = ClassLoader.getSystemClassLoader.loadClass(s)
      println(ClassLayout.parseClass(klass).nn.toPrintable)
    catch _ => println(s"${Console.RED}Invalid class: $s${Console.RESET}\n")
