package turbolift.effects
import turbolift.{!!, Signature, Effect}


trait ContSignature[R] extends Signature:
  def shift[A, U <: ThisEffect](f: (A => R !! U) => R !! U): A !! U
  def reset[U <: ThisEffect](body: R !! U): R !! U


trait Cont[R] extends Effect[ContSignature[R]] with ContSignature[R]:
  final override def shift[A, U <: this.type](f: (A => R !! U) => R !! U): A !! U = perform(_.shift(f))
  final override def reset[U <: this.type](body: R !! U): R !! U = perform(_.reset(body))


  def handler: ThisHandler[Const[R], Const[R], Any] =
    new impl.Stateless[Const[R], Const[R], Any] with impl.Sequential with ContSignature[R]:
      override def onReturn(r: R) = !!.pure(r)

      override def shift[A, U <: ThisEffect](f: (A => R !! U) => R !! U): A !! U = Control.capture(f)

      override def reset[U <: ThisEffect](body: R !! U): R !! U = Control.delimit(body)

    .toHandler
