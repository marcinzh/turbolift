package turbolift.internals.executor
import java.util.concurrent.{Executors, ThreadFactory}


private[internals] object Scheduler:
  val instance = Executors.newSingleThreadScheduledExecutor(makeThreadFactory).nn

  private def makeThreadFactory: ThreadFactory =
    new ThreadFactory:
      override def newThread(runnable: Runnable): Thread =
        val thread = new Thread(runnable)
        thread.setName("turbolift-scheduler")
        thread.setDaemon(true)
        thread.setPriority(Thread.MAX_PRIORITY)
        thread
