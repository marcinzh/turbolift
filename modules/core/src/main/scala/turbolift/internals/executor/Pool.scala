package turbolift.internals.executor
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{Executors, ExecutorService, ThreadFactory}


private[internals] object Pool:
  val instance: ExecutorService = Executors.newCachedThreadPool(makeThreadFactory).nn

  private def makeThreadFactory: ThreadFactory =
    new AtomicLong with ThreadFactory:
      override def newThread(runnable: Runnable): Thread =
        val n = getAndIncrement()
        val thread = new Thread(runnable, s"turbolift-$n")
        thread.setDaemon(true)
        thread
