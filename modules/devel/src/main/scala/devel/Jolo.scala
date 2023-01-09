package devel
import org.openjdk.jol.info.ClassLayout
import org.openjdk.jol.vm.VM


object Jolo:
  def run =
    val klass = ClassLoader.getSystemClassLoader.loadClass("turbolift.internals.engine.Fiber")
    println(VM.current.nn.details)
    println(ClassLayout.parseClass(klass).nn.toPrintable)
