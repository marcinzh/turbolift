package turbolift.extra_effects
import turbolift.{!!, Signature, Effect}


trait ContSignature[R] extends Signature:
  def shift[A, U <: ThisEffect](f: (A => R !! U) => R !! U): A !@! U
  def reset[U <: ThisEffect](body: R !! U): R !@! U


trait Cont[R] extends Effect[ContSignature[R]] with ContSignature[R]:
  final override def shift[A, U <: this.type](f: (A => R !! U) => R !! U): A !! U = perform(_.shift(f))
  final override def reset[U <: this.type](body: R !! U): R !! U = perform(_.reset(body))


  def handler: ThisHandler[[_] =>> R, [_] =>> R, Any] =
    new impl.Free.Const.Stateless[R, [_] =>> R] with impl.Sequential with ContSignature[R]:
      override def onReturn[A](r: R) = !!.pure(r)

      override def shift[A, U <: ThisEffect](f: (A => R !! U) => R !! U): A !@! U =
        k => k.escape(f(k)).map(_._1)

      override def reset[U <: ThisEffect](body: R !! U): R !@! U =
        k => k.local(body).flatMap:
          case (r, k2) => k2(r)

    .toHandler
