package devel
import org.openjdk.jol.info.ClassLayout
import org.openjdk.jol.vm.VM


object Jolo:
  def run =
    println(VM.current.nn.details)
    """
      turbolift.internals.engine.FiberImpl
      turbolift.internals.executor.MultiThreadedExecutor
    """
    .split("\n").map(_.trim).filter(_.nonEmpty)
    .foreach(show)


  def show(s: String) =
    try
      val klass = ClassLoader.getSystemClassLoader.loadClass(s)
      println(ClassLayout.parseClass(klass).nn.toPrintable)
    catch _ => println(s"${Console.RED}Invalid class: $s${Console.RESET}\n")
