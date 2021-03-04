package turbolift.abstraction
import scala.util.{Try, Success, Failure}
import cats.{Id, ~>}


private [abstraction] trait HandlerExtensions {
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

    def ***![S2, L2, N2](that: Handler[(S2, *), L2, N2]) =
      thiz.composeWith(that)
      .map(new (Lambda[X => (S2, (S, X))] ~> Lambda[X => ((S, S2), X)]) {
        def apply[A](pairs: (S2, (S, A))): ((S, S2), A) = {
          val (s2, (s, a)) = pairs
          ((s, s2), a)
        }
      })
  }

  implicit class HandlerExtension_Option[L, N](thiz: Handler[Option, L, N]) {
    def toEither[E](e: => E): Handler[Either[E, *], L, N] =
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
