package turbolift.abstraction
import cats.{Id, ~>}
import turbolift.abstraction.internals.aux.CanPartiallyHandle
import turbolift.abstraction.internals.interpreter.MonadTransformer


sealed trait Handler[Result[_], Elim] {
  def doHandle[A, U](comp: A !! U with Elim): Result[A] !! U

  final def handle[V] = new HandleApply[V]
  final class HandleApply[V] {
    def apply[A, W](comp: A !! W)(implicit ev: CanPartiallyHandle[V, W, Elim]): Result[A] !! V =
      doHandle[A, V](ev(comp))
  }

  final def run[A](comp: A !! Elim): Result[A] = handle[Any](comp).run

  final def map[NewResult[_]](f: Result ~> NewResult): Handler[NewResult, Elim] =
    HandlerCases.Mapped[Result, NewResult, Elim](this, f)

  final def composeWith[ThatResult[_], ThatElim](that: Handler[ThatResult, ThatElim]) =
    HandlerCases.Composed[Result, ThatResult, Elim, ThatElim](this, that).self
  
  final def <<<![ThatResult[_], ThatElim](that: Handler[ThatResult, ThatElim]) = that.composeWith(this)
  final def >>>![ThatResult[_], ThatElim](that: Handler[ThatResult, ThatElim]) = this.composeWith(that)

  final def self: Handler[Result, Elim] = this
}


object Handler extends HandlerExtensions


private[abstraction] object HandlerCases {
  sealed trait Primitive[Result[_], Elim] extends Handler[Result, Elim] {
    type Trans[M[_], A]
    def prime[M[_], A](tma: Trans[M, A]): M[Result[A]]
    def transformer: MonadTransformer[Trans, Result]
    final override def doHandle[A, U](comp: A !! U with Elim): Result[A] !! U =
      new ComputationCases.Scope[A, U, Result, Elim](comp, this)
  }

  final case class Nullary[Result[_], Elim](
    transformer: MonadTransformer[Lambda[(`M[_]`, A) => M[Result[A]]], Result]
  ) extends Primitive[Result, Elim] {
    override type Trans[M[_], A] = M[Result[A]]
    override def prime[M[_], A](tma: M[Result[A]]): M[Result[A]] = tma
  }

  final case class Unary[S, Result[_], Elim](
    transformer: MonadTransformer[Lambda[(`M[_]`, A) => S => M[Result[A]]], Result],
    initial: S
  ) extends Primitive[Result, Elim] {
    override type Trans[M[_], A] = S => M[Result[A]]
    override def prime[M[_], A](tma: S => M[Result[A]]): M[Result[A]] = tma(initial)
  }

  final case class Composed[Result1[_], Result2[_], Elim1, Elim2](
    first: Handler[Result1, Elim1],
    second: Handler[Result2, Elim2],
  ) extends Handler[Lambda[X => Result2[Result1[X]]], Elim1 with Elim2] {
    override def doHandle[A, U](comp: A !! U with Elim1 with Elim2): Result2[Result1[A]] !! U =
      second.doHandle[Result1[A], U](
        first.doHandle[A, U with Elim2](comp)
      )
  }

  final case class Mapped[OldResult[_], NewResult[_], Elim](
    that: Handler[OldResult, Elim],
    fun: OldResult ~> NewResult,
  ) extends Handler[NewResult, Elim] {
    override def doHandle[A, U](comp: A !! U with Elim): NewResult[A] !! U =
      that.doHandle[A, U](comp).map(fun(_))
  }
}


trait HandlerExports {
}


trait HandlerExtensions {
  implicit class HandlerExtension_Pair[S, U](val thiz: Handler[(S, ?), U]) {
    type Const[X] = S

    def eval: Handler[Id, U] = thiz.map(new ((S, ?) ~> Id) {
      def apply[A](pair: (S, A)) = pair._2
    })

    def exec: Handler[Const, U] = thiz.map[Const](new ((S, ?) ~> Const) {
      def apply[A](pair: (S, A)) = pair._1
    })

    def justState = exec
    def dropState = eval
  }
}
