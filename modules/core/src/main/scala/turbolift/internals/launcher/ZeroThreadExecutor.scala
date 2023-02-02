package turbolift.internals.launcher
import java.util.LinkedList
import java.util.concurrent.Executor


private[launcher] final class ZeroThreadedExecutor extends Executor:
  private val queue = new LinkedList[Runnable]

  override def execute(r: Runnable): Unit = queue.addLast(r)

  def kill(): Unit = queue.clear()

  def drainAll(): Unit =
    while !queue.isEmpty do
      queue.removeFirst.nn.run()
