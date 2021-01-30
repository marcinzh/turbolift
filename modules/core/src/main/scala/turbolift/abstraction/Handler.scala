package turbolift.abstraction
import scala.util.{Try, Success, Failure}
import cats.{Id, ~>}
import turbolift.abstraction.internals.aux.CanPartiallyHandle
import turbolift.abstraction.internals.interpreter.Interpreter


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
  
  final def provideWith[ThatResult[_], ThatIntro](that: Handler[ThatResult, Intro, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, Any, Any, ThatIntro, Intro](Handler.this, that).self

  final def partiallyProvideWith_[ThatResult[_], ThatIntro](that: Handler[ThatResult, Intro, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, Any, Any, ThatIntro, Intro](Handler.this, that).self

  final def partiallyProvideWith[Remains >: Intro] = new PartiallyProvideWithApply[Remains]
  class PartiallyProvideWithApply[Remains >: Intro] {
    def apply[ThatResult[_], ThatElim >: Intro, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
     HandlerCases.Composed[Result, ThatResult, Elim, Any, Remains, ThatIntro, ThatElim](upCastIntro[Remains with ThatElim], that).self
  }

  final def upCastIntro[T >: Intro] = asInstanceOf[Handler[Result, Elim, T]]

  final def <<<![ThatResult[_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = that.composeWith(this)
  final def >>>![ThatResult[_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = this.composeWith(that)

  final def self: Handler[Result, Elim, Intro] = this

  final def void: Handler[Lambda[X => Unit], Elim, Intro] =
    map[Lambda[X => Unit]](new ~>[Result, Lambda[X => Unit]] {
      def apply[A](fa: Result[A]): Unit = ()
    })
}


object Handler extends HandlerExtensions


private[abstraction] object HandlerCases {
  final case class Primitive[Result[_], Elim, Intro](
    interpreter: Interpreter.Saturated[Result, Elim, Intro]
  ) extends Handler[Result, Elim, Intro] {
    override def doHandle[A, U](comp: A !! U with Elim): Result[A] !! U with Intro = 
      new ComputationCases.Scope[A, U, Result, Elim, Intro](comp, this)
  }

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
  implicit class HandlerExtension_Pair[S, L, N](val thiz: Handler[(S, *), L, N]) {
    type Const[X] = S

    def eval: Handler[Id, L, N] =
      thiz.map(new ((S, *) ~> Id) {
        def apply[A](pair: (S, A)): A = pair._2
      })

    def exec: Handler[Const, L, N] =
      thiz.map[Const](new ((S, *) ~> Const) {
        def apply[A](pair: (S, A)): S = pair._1
      })

    def justState = exec
    def dropState = eval

    def mapState[S2](f: S => S2): Handler[(S2, *), L, N] =
      thiz.map(new ((S, *) ~> (S2, *)) {
        def apply[A](pair: (S, A)): (S2, A) = {
          val (s, a) = pair
          (f(s), a)
        }
      })
  }

  implicit class HandlerExtension_NestedPairs[S1, S2, L, N](val thiz: Handler[Lambda[X => (S1, (S2, X))], L, N]) {
    def joinStates: Handler[((S1, S2), *), L, N] =
      thiz.map(new (Lambda[X => (S1, (S2, X))] ~> ((S1, S2), *)) {
        def apply[A](pairs: (S1, (S2, A))): ((S1, S2), A) = {
          val (s1, (s2, a)) = pairs
          ((s1, s2), a)
        }
      })
  }

  implicit class HandlerExtension_Option[L, N](thiz: Handler[Option, L, N]) {
    def toEither[E](e : => E): Handler[Either[E, *], L, N] =
      thiz.map(new (Option ~> Either[E, *]) {
        override def apply[A](result: Option[A]): Either[E, A] = result.toRight(e)
      })

    def toTry(e: => Throwable): Handler[Try, L, N] = 
      thiz.map(new (Option ~> Try) {
        override def apply[A](result: Option[A]): Try[A] = result.fold[Try[A]](Failure(e))(Success(_))
      })
  }

  implicit class HandlerExtension_Either[E, L, N](thiz: Handler[Either[E, *], L, N]) {
    def toOption: Handler[Option, L, N] =
      thiz.map(new (Either[E, *] ~> Option) {
        override def apply[A](result: Either[E, A]): Option[A] = result.toOption
      })

    def toTry(implicit ev: E <:< Throwable): Handler[Try, L, N] = 
      thiz.map(new (Either[E, *] ~> Try) {
        override def apply[A](result: Either[E, A]): Try[A] = result.toTry
      })

    def mapLeft[E2](f: E => E2): Handler[Either[E2, *], L, N] =
      thiz.map(new (Either[E, *] ~> Either[E2, *]) {
        override def apply[A](result: Either[E, A]): Either[E2, A] = result.swap.map(f).swap
      })
  }
}
