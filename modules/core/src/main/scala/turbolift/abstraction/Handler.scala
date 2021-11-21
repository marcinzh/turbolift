package turbolift.abstraction
import turbolift.abstraction.internals.aux.CanPartiallyHandle
import turbolift.abstraction.internals.interpreter.Interpreter


sealed trait Handler[Result[+_], Elim, Intro]:
  def doHandle[A, U](comp: A !! U with Elim): Result[A] !! U with Intro

  final def handle[V] = new HandleApply[V]
  final class HandleApply[V]:
    def apply[A, W](comp: A !! W)(implicit ev: CanPartiallyHandle[V, W, Elim]): Result[A] !! V with Intro =
      doHandle[A, V](ev(comp))

  final def run[A](comp: A !! Elim)(implicit ev: Intro =:= Any): Result[A] = handle[Any](comp).run

  final def map[NewResult[+_]](f: [X] => Result[X] => NewResult[X]): Handler[NewResult, Elim, Intro] =
    HandlerCases.Mapped[Result, NewResult, Elim, Intro](this, f)

  final def composeWith[ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, ThatElim, Intro, ThatIntro, Any](this, that).self
  
  final def provideWith[ThatResult[+_], ThatIntro](that: Handler[ThatResult, Intro, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, Any, Any, ThatIntro, Intro](Handler.this, that).self

  final def partiallyProvideWith_[ThatResult[+_], ThatIntro](that: Handler[ThatResult, Intro, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, Any, Any, ThatIntro, Intro](Handler.this, that).self

  final def partiallyProvideWith[Remains >: Intro] = new PartiallyProvideWithApply[Remains]
  class PartiallyProvideWithApply[Remains >: Intro]:
    def apply[ThatResult[+_], ThatElim >: Intro, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
     HandlerCases.Composed[Result, ThatResult, Elim, Any, Remains, ThatIntro, ThatElim](upCastIntro[Remains with ThatElim], that).self

  final def upCastIntro[T >: Intro] = asInstanceOf[Handler[Result, Elim, T]]

  final def <<<![ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = that.composeWith(this)
  final def >>>![ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = this.composeWith(that)

  final def self: Handler[Result, Elim, Intro] = this

  final def void: Handler[[X] =>> Unit, Elim, Intro] = map([X] => (_: Result[X]) => ())



object Handler extends HandlerExtensions:
  type Id[Elim, Intro] = Handler[[X] =>> X, Elim, Intro]
  type IId[Elim] = Id[Elim, Any]


private[abstraction] object HandlerCases:
  final case class Primitive[Result[+_], Elim, Intro](
    interpreter: Interpreter.Apply[Result, Elim, Intro],
    initial: Any,
  ) extends Handler[Result, Elim, Intro]:
    override def doHandle[A, U](comp: A !! U with Elim): Result[A] !! U with Intro = 
      new ComputationCases.Delimit[A, U, Result, Elim, Intro](comp, this)


  final case class Composed[Result1[+_], Result2[+_], Elim1, Elim2, Intro1, Intro2, Hidden](
    first: Handler[Result1, Elim1, Intro1 with Hidden],
    second: Handler[Result2, Elim2 with Hidden, Intro2],
  ) extends Handler[[X] =>> Result2[Result1[X]], Elim1 with Elim2, Intro1 with Intro2]:
    override def doHandle[A, U](comp: A !! U with Elim1 with Elim2): Result2[Result1[A]] !! U with Intro1 with Intro2 =
      second.doHandle[Result1[A], U with Intro1](
        first.doHandle[A, U with Elim2](comp)
      )
  

  final case class Mapped[OldResult[+_], NewResult[+_], Elim, Intro](
    that: Handler[OldResult, Elim, Intro],
    fun: [X] => OldResult[X] => NewResult[X],
  ) extends Handler[NewResult, Elim, Intro]:
    override def doHandle[A, U](comp: A !! U with Elim): NewResult[A] !! U with Intro =
      that.doHandle[A, U](comp).map(fun(_))


type IHandler[F[+_], L] = Handler[F, L, Any]
