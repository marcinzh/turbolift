package turbolift.effects
import turbolift.{!!, Signature, Effect}


trait ContSignature[R] extends Signature:
  def shift[A, U <: ThisEffect](f: (A => R !! U) => R !! U): A !@! U
  def reset[U <: ThisEffect](body: R !! U): R !@! U


trait Cont[R] extends Effect[ContSignature[R]] with ContSignature[R]:
  final override def shift[A, U <: this.type](f: (A => R !! U) => R !! U): A !! U = perform(_.shift(f))
  final override def reset[U <: this.type](body: R !! U): R !! U = perform(_.reset(body))


  def handler: ThisHandler.FromConst.ToConst.Free[R, R] =
    new impl.Stateless.FromConst.ToConst.Free[R, R] with impl.Sequential with ContSignature[R]:
      override def topmostOnlyHint = true

      override def onReturn(r: R) = !!.pure(r)

      override def shift[A, U <: ThisEffect](f: (A => R !! U) => R !! U): A !@! U =
        k => k.escapeAndForget(f(k.resume(_)))

      override def reset[U <: ThisEffect](body: R !! U): R !@! U =
        k => k.localAndResume(body)

    .toHandler
