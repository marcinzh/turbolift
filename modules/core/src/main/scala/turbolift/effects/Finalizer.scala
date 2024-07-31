package turbolift.effects
import java.io.{Closeable => JCloseable}
import turbolift.{!!, Effect, Signature, Handler}
import turbolift.effects.IO
import turbolift.handlers.finalizerHandler_IO
import turbolift.io.ResourceFactory
import turbolift.Extensions._


trait FinalizerSignature[U] extends Signature:
  def use[A](acquire: A !! U, release: A => Unit !! U): A !! ThisEffect


trait FinalizerEffect[U] extends Effect[FinalizerSignature[U]] with FinalizerSignature[U]:
  final override def use[A](acquire: A !! U, release: A => Unit !! U): A !! this.type = perform(_.use(acquire, release))

  final def use[A, U2 >: U](rf: ResourceFactory[A, U2]): A !! this.type =
    use(rf.acquire, rf.release(_))


object FinalizerEffect:
  extension [U <: IO](thiz: FinalizerEffect[U])
    def use[A <: JCloseable](acquire: => A): A !! thiz.type =
      thiz.use(IO(acquire), a => IO(a.close))

    def handlerIO: Handler[Identity, Identity, thiz.type, U] =
      thiz.finalizerHandler_IO


trait FinalizerEffectIO[U <: IO] extends FinalizerEffect[U]:
  /** Default handler for this effect. */
  def handler = this.handlerIO

  def scoped[A, V](comp: A !! (V & this.type)): A !! (V & U) = comp.handleWith(handler)


/** Predefined instance of this effect. */
case object Finalizer extends FinalizerEffectIO[IO]

type Finalizer = Finalizer.type
