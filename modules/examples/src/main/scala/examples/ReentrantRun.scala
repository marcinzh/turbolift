package examples
import java.util.concurrent.atomic.AtomicInteger
import turbolift.!!
import turbolift.Extensions._
import turbolift.internals.executor.Executor


case object ReentrantRun extends Example:
  override def description: String = """
    Shows that `run` is reentrant, at the cost of forcing the thread pool to grow.
    Thanks to this feature, running a non-IO computation can be treated as pure expression.
    Even though the computation may be internally parallellized
    (without explicit use of fibers, e.g. `zipPar`),
    running it stays total and deterministic.
  """

  val CPUS = Runtime.getRuntime.nn.availableProcessors()
  val DELAY = 1000
  val TIMES = 10

  val log =
    val counter = new AtomicInteger(0)
    (s: String) => !!.impure {
      print(s)
      if counter.updateAndGet(i => (i + 1) % CPUS) == 0 then
        println
    }

  val sleep = !!.impure(Thread.sleep(DELAY))

  def hog(id: Int): Unit !! Any =
    !!.repeat(TIMES) {
      log(s"[${Console.GREEN}Hog ${id}${Console.RESET}]") &&!
      sleep
    }

  def dig: Unit !! Any =
    def loop(i: Int): Unit !! Any =
      !!.when(i < TIMES) {
        log(s"[${Console.RED}Reentry @depth=$i${Console.RESET}]") &&!
        sleep &&!
        !!.impure {
          loop(i + 1).run
        }
      }
    loop(0)


  override def apply() =
    println(s"$CPUS CPUs found.")
    val hogs = (1 until CPUS).map(hog)
    Executor.reentrant.runSync:
      (dig +: hogs).traverseVoidPar
