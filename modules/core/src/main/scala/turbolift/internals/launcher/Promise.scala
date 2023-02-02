package turbolift.internals.launcher
import java.util.concurrent.CountDownLatch
import scala.util.{Try, Success, Failure}
import turbolift.internals.engine.Panic


private[internals] sealed trait Promise[A]:
  protected var result: Try[A] | Null = null

  def set(result: Try[A]): Unit
  def get: Try[A]

  final def success(value: A): Unit = set(Success(value))
  final def failure(throwable: Throwable): Unit = set(Failure(throwable))
  final def untyped = asInstanceOf[Promise.Untyped]

  final def panicked: Boolean =
    result match
      case Failure(_: Panic) => true
      case _ => false


private[internals] object Promise:
  type Untyped = Promise[Any]

  def singleThreaded[A](): Promise[A] =
    new Promise[A]:
      override def set(result: Try[A]): Unit = this.result = result
      override def get: Try[A] = result.nn


  def multiThreaded[A](): Promise[A] =
    new CountDownLatch(1) with Promise[A]:
      override def set(result: Try[A]): Unit =
        //@#@YOLO
        this.result = result
        countDown()

      override def get: Try[A] =
        await()
        result.nn
