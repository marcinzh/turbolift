package turbolift.abstraction
import scala.util.Try
import cats.{Id, ~>}
import turbolift.abstraction.internals.aux.CanPartiallyHandle
import turbolift.abstraction.internals.interpreter.MonadTransformer


sealed trait Handler[Result[_], Elim, Intro] {
  def doHandle[A, U](comp: A !! U with Elim): Result[A] !! U with Intro

  final def handle[V] = new HandleApply[V]
  final class HandleApply[V] {
    def apply[A, W](comp: A !! W)(implicit ev: CanPartiallyHandle[V, W, Elim]): Result[A] !! V with Intro =
      doHandle[A, V](ev(comp))
  }

  final def run[A](comp: A !! Elim)(implicit ev: Intro =:= Any): Result[A] = handle[Any](comp).run

  final def map[NewResult[_]](f: Result ~> NewResult): Handler[NewResult, Elim, Intro] =
    HandlerCases.Mapped[Result, NewResult, Elim, Intro](this, f)

  final def composeWith[ThatResult[_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, ThatElim, Intro, ThatIntro, Any](this, that).self
  
  final def <<<![ThatResult[_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = that.composeWith(this)
  final def >>>![ThatResult[_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = this.composeWith(that)

  final def self: Handler[Result, Elim, Intro] = this
}


object Handler extends HandlerExtensions


private[abstraction] object HandlerCases {
  sealed trait Primitive[Result[_], Elim, Intro] extends Handler[Result, Elim, Intro] {
    type Trans[M[_], A]
    def prime[M[_], A](tma: Trans[M, A]): M[Result[A]]
    def transformer: MonadTransformer[Trans, Result]
    final override def doHandle[A, U](comp: A !! U with Elim): Result[A] !! U with Intro =
      new ComputationCases.Scope[A, U, Result, Elim, Intro](comp, this)
  }

  final case class Nullary[Result[_], Elim, Intro](
    transformer: MonadTransformer[Lambda[(`M[_]`, A) => M[Result[A]]], Result]
  ) extends Primitive[Result, Elim, Intro] {
    override type Trans[M[_], A] = M[Result[A]]
    override def prime[M[_], A](tma: M[Result[A]]): M[Result[A]] = tma
  }

  final case class Unary[S, Result[_], Elim, Intro](
    transformer: MonadTransformer[Lambda[(`M[_]`, A) => S => M[Result[A]]], Result],
    initial: S
  ) extends Primitive[Result, Elim, Intro] {
    override type Trans[M[_], A] = S => M[Result[A]]
    override def prime[M[_], A](tma: S => M[Result[A]]): M[Result[A]] = tma(initial)
  }

  // final case class Composed[Result1[_], Result2[_], Elim, Intro1, Elim, Intro2](
  //   first: Handler[Result1, Elim, Intro1],
  //   second: Handler[Result2, Elim, Intro2],
  // ) extends Handler[Lambda[X => Result2[Result1[X]]], Elim, Intro1 with Elim2] {
  //   override def doHandle[A, U](comp: A !! U with Elim1 with Elim2): Result2[Result1[A]] !! U =
  //     second.doHandle[Result1[A], U](
  //       first.doHandle[A, U with Elim2](comp)
  //     )
  // }
  final case class Composed[Result1[_], Result2[_], Elim1, Elim2, Intro1, Intro2, Hidden](
    first: Handler[Result1, Elim1, Intro1 with Hidden],
    second: Handler[Result2, Elim2 with Hidden, Intro2],
  ) extends Handler[Lambda[X => Result2[Result1[X]]], Elim1 with Elim2, Intro1 with Intro2] {
    override def doHandle[A, U](comp: A !! U with Elim1 with Elim2): Result2[Result1[A]] !! U with Intro1 with Intro2 =
      second.doHandle[Result1[A], U with Intro1](
        first.doHandle[A, U with Elim2](comp)
      )
  }

  final case class Mapped[OldResult[_], NewResult[_], Elim, Intro](
    that: Handler[OldResult, Elim, Intro],
    fun: OldResult ~> NewResult,
  ) extends Handler[NewResult, Elim, Intro] {
    override def doHandle[A, U](comp: A !! U with Elim): NewResult[A] !! U with Intro =
      that.doHandle[A, U](comp).map(fun(_))
  }
}


trait HandlerExports {
  type IHandler[F[_], L] = Handler[F, L, Any]
}


trait HandlerExtensions {
  implicit class HandlerExtension_Pair[S, L, N](val thiz: Handler[(S, ?), L, N]) {
    type Const[X] = S

    def eval: Handler[Id, L, N] = thiz.map(new ((S, ?) ~> Id) {
      def apply[A](pair: (S, A)) = pair._2
    })

    def exec: Handler[Const, L, N] = thiz.map[Const](new ((S, ?) ~> Const) {
      def apply[A](pair: (S, A)) = pair._1
    })

    def justState = exec
    def dropState = eval
  }

  implicit class HandlerExtension_Option[L, N](thiz: Handler[Option, L, N]) {
    def toEither[E](e : => E): Handler[Either[E, ?], L, N] =
      thiz.map(new (Option ~> Either[E, ?]) {
        override def apply[A](result: Option[A]) = result.toRight(e)
      })
  }

  implicit class HandlerExtension_Either[E, L, N](thiz: Handler[Either[E, ?], L, N]) {
    def toOption: Handler[Option, L, N] =
      thiz.map(new (Either[E, ?] ~> Option) {
        override def apply[A](result: Either[E, A]) = result.toOption
      })

    def toTry(implicit ev: E <:< Throwable): Handler[Try, L, N] = 
      thiz.map(new (Either[E, ?] ~> Try) {
        override def apply[A](result: Either[E, A]) = result.toTry
      })
  }
}
