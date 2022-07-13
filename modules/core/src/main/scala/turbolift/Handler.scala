package turbolift
import turbolift.internals.aux.CanPartiallyHandle
import turbolift.internals.extensions.HandlerExtensions
import turbolift.internals.interpreter.Interpreter


sealed trait Handler[Result[+_], Elim, Intro]:
  def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro)

  final def handle[V] = new HandleApply[V]
  final class HandleApply[V]:
    def apply[A, W](comp: A !! W)(implicit ev: CanPartiallyHandle[V, W, Elim]): Result[A] !! (V & Intro) =
      doHandle[A, V](ev(comp))

  final def run[A](comp: A !! Elim)(implicit ev: Intro =:= Any): Result[A] = handle[Any](comp).run

  final def map[NewResult[+_]](f: [X] => Result[X] => NewResult[X]): Handler[NewResult, Elim, Intro] =
    HandlerCases.Mapped[Result, NewResult, Elim, Intro](this, f)

  final def flatMap[NewResult[+_], V](f: [X] => Result[X] => NewResult[X] !! V): Handler[NewResult, Elim, Intro & V] =
    HandlerCases.FlatMapped[Result, NewResult, Elim, Intro, V](this, f)

  final def flatTap[V](f: [X] => Result[X] => Unit !! V): Handler[Result, Elim, Intro & V] =
    HandlerCases.FlatTapped[Result, Elim, Intro, V](this, f)

  final def composeWith[ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, ThatElim, Intro, ThatIntro, Any](this, that).self
  
  final def provideWith[ThatResult[+_], ThatIntro](that: Handler[ThatResult, Intro, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, Any, Any, ThatIntro, Intro](Handler.this, that).self

  final def partiallyProvideWith[Remains >: Intro] = new PartiallyProvideWithApply[Remains]
  class PartiallyProvideWithApply[Remains >: Intro]:
    def apply[ThatResult[+_], ThatElim >: Intro, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
     HandlerCases.Composed[Result, ThatResult, Elim, Any, Remains, ThatIntro, ThatElim](upCastIntro[Remains & ThatElim], that).self

  final def upCastIntro[T >: Intro] = asInstanceOf[Handler[Result, Elim, T]]

  final def &&&![ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = this.composeWith(that)

  final def self: Handler[Result, Elim, Intro] = this

  final def void: Handler[[X] =>> Unit, Elim, Intro] = map([X] => (_: Result[X]) => ())



object Handler extends HandlerExtensions:
  type Id[Elim, Intro] = Handler[[X] =>> X, Elim, Intro]
  type Const[Result, Elim, Intro] = Handler[[X] =>> Result, Elim, Intro]
  type Free[Result[+_], Elim] = Handler[Result, Elim, Any]
  type FreeId[Elim] = Handler[[X] =>> X, Elim, Any]
  type FreeConst[Result, Elim] = Handler[[X] =>> Result, Elim, Any]

  def flatHandle[F[+_], L, N1, N2](h: Handler[F, L, N1] !! N2): Handler[F, L, N1 & N2] = HandlerCases.FlatHandled(h)


private[turbolift] object HandlerCases:
  final case class Primitive[Result[+_], Elim, Intro](
    interpreter: Interpreter.Apply[Result, Elim, Intro],
    initial: Any,
  ) extends Handler[Result, Elim, Intro]:
    override def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro) = 
      new ComputationCases.Delimit[A, U, Result, Elim, Intro](comp, this)


  final case class Composed[Result1[+_], Result2[+_], Elim1, Elim2, Intro1, Intro2, Hidden](
    first: Handler[Result1, Elim1, Intro1 & Hidden],
    second: Handler[Result2, Elim2 & Hidden, Intro2],
  ) extends Handler[[X] =>> Result2[Result1[X]], Elim1 & Elim2, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim1 & Elim2)): Result2[Result1[A]] !! (U & Intro1 & Intro2) =
      second.doHandle[Result1[A], U & Intro1](
        first.doHandle[A, U & Elim2](comp)
      )
  

  final case class Mapped[OldResult[+_], NewResult[+_], Elim, Intro](
    that: Handler[OldResult, Elim, Intro],
    fun: [X] => OldResult[X] => NewResult[X],
  ) extends Handler[NewResult, Elim, Intro]:
    override def doHandle[A, U](comp: A !! (U & Elim)): NewResult[A] !! (U & Intro) =
      that.doHandle[A, U](comp).map(fun(_))


  final case class FlatMapped[OldResult[+_], NewResult[+_], Elim, Intro1, Intro2](
    that: Handler[OldResult, Elim, Intro1],
    fun: [X] => OldResult[X] => NewResult[X] !! Intro2,
  ) extends Handler[NewResult, Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim)): NewResult[A] !! (U & Intro1 & Intro2) =
      that.doHandle[A, U](comp).flatMap(fun(_))


  final case class FlatTapped[Result[+_], Elim, Intro1, Intro2](
    that: Handler[Result, Elim, Intro1],
    fun: [X] => Result[X] => Unit !! Intro2,
  ) extends Handler[Result, Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro1 & Intro2) =
      that.doHandle[A, U](comp).flatTap(fun(_))


  final case class FlatHandled[Result[+_], Elim, Intro1, Intro2](
    that: Handler[Result, Elim, Intro1] !! Intro2,
  ) extends Handler[Result[+_], Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro1 & Intro2) =
      that.flatMap(_.doHandle[A, U](comp))
