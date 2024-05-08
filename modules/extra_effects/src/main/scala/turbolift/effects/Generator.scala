package turbolift.effects
import turbolift.{!!, Signature, Effect}
import turbolift.Extensions._
import Generator.Step


opaque type Generator[+A, -U] = Generator.Step[A, U] !! U

object Generator:
  enum Step[+A, -U]:
    case Yield(value: A, more: Step[A, U] !! U)
    case End

  def apply[A, U](step: Step[A, U] !! U): Generator[A, U] = step

  def producer[A, U](body: (fx: ProducerEffect[A]) => Unit !! (U & fx.type)): Generator[A, U] =
    case object Fx extends ProducerEffect[A]
    body(Fx).handleWith(Fx.handler)

  def consumer[A, U](body: (fx: ConsumerEffect[A]) => Unit !! (U & fx.type)): Generator[A, U] => Unit !! U =
    case object Fx extends ConsumerEffect[A]
    gen => body(Fx).handleWith(Fx.handler(gen))

  extension [A, U](thiz: Generator[A, U])
    def step: Step[A, U] !! U = thiz

    def toVector: Vector[A] !! U =
      def loop(todo: Step[A, U] !! U, accum: Vector[A]): Vector[A] !! U =
        todo.flatMap:
          case Step.Yield(a, more) => loop(more, accum :+ a)
          case Step.End => accum.pure_!!
      loop(thiz, Vector())

    def take(count: Long): Generator[A, U] =
      def loop(todo: Step[A, U] !! U, count: Long): Step[A, U] !! U =
        if count > 0 then
          todo.map:
            case Step.Yield(a, more) => Step.Yield(a, loop(more, count - 1))
            case Step.End => Step.End
        else
          Step.End.pure_!!
      loop(thiz, count)


trait ProducerSignature[A] extends Signature:
  def yeld(value: A): Unit !! ThisEffect


trait ProducerEffect[A] extends Effect[ProducerSignature[A]] with ProducerSignature[A]:
  final override def yeld(value: A): Unit !! this.type = perform(_.yeld(value))

  final def handler[U]: ThisHandler[Const[Unit], Const[Step[A, U]], Any] =
    new impl.Stateless[Const[Unit], Const[Step[A, U]], Any] with impl.Sequential with ProducerSignature[A]:
      override def onReturn(aa: Unit): Step[A, U] !! Any =
        Step.End.pure_!!

      override def yeld(value: A): Unit !! ThisEffect =
        Control.capture: k =>
          Step.Yield(value, k.resume(())).pure_!!

    .toHandler


trait ConsumerSignature[A] extends Signature:
  def await: A !! ThisEffect


trait ConsumerEffect[A] extends Effect[ConsumerSignature[A]] with ConsumerSignature[A]:
  final override def await: A !! this.type = perform(_.await)

  final def handler[U](initial: Step[A, U] !! U): ThisHandler[Const[Unit], Const[Unit], U] =
    new impl.Stateful[Const[Unit], Const[Unit], U] with impl.Sequential with ConsumerSignature[A]:
      override type Local = Step[A, U] !! U

      override def onInitial = initial.pure_!!

      override def onReturn(a: Unit, s: Local) = !!.unit

      override def await: A !! ThisEffect =
        Control.captureGet: (k, s) =>
          s.flatMap:
            case Step.End => !!.unit
            case Step.Yield(a, s2) => k(a, s2)

    .toHandler
