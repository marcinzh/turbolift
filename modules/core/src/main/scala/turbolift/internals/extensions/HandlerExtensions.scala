package turbolift.internals.extensions
import turbolift.Handler
import scala.util.{Try, Success, Failure}


trait HandlerExtensions:
  extension [S, L, N](thiz: Handler[(S, _), L, N])
    def eval: Handler.Id[L, N] = dropState

    def exec: Handler[[X] =>> S, L, N] = justState

    def justState: Handler[[X] =>> S, L, N] = thiz.map([A] => (pair: (S, A)) => pair._1)

    def dropState: Handler.Id[L, N] = thiz.map([A] => (pair: (S, A)) => pair._2)

    def mapState[S2](f: S => S2): Handler[(S2, _), L, N] =
      thiz.map([A] => (pair: (S, A)) =>
        val (s, a) = pair
        (f(s), a)
      )

    def ***![S2, L2, N2](that: Handler[(S2, _), L2, N2]): Handler[((S, S2), _), L & L2, N & N2] =
      thiz.composeWith(that).map([A] => (pairs: (S2, (S, A))) =>
        val (s2, (s, a)) = pairs
        ((s, s2), a)
      )


  extension [L, N](thiz: Handler[Option, L, N])
    def toVector: Handler[Vector, L, N] =
      thiz.map([A] => (result: Option[A]) => result.toVector)
    
    def toEither[E](e: => E): Handler[Either[E, _], L, N] =
      thiz.map([A] => (result: Option[A]) => result.toRight(e))

    def toTry(e: => Throwable): Handler[Try, L, N] =
      thiz.map([A] => (result: Option[A]) => result.fold[Try[A]](Failure(e))(Success(_)))

    def getOrElse(default: => Nothing): Handler.Id[L, N] =
      thiz.map([A] => (result: Option[A]) => result.getOrElse(default))

    def getOrDie(message: => String): Handler.Id[L, N] =
      getOrElse(sys.error(message))

    def unsafeGet: Handler.Id[L, N] =
      thiz.map([A] => (result: Option[A]) => result.get)


  extension [E, L, N](thiz: Handler[Either[E, _], L, N])
    def toOption: Handler[Option, L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.toOption)

    def toTry(implicit ev: E <:< Throwable): Handler[Try, L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.toTry)

    def getOrElse(default: E => Nothing): Handler.Id[L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.fold(default, x => x))

    def getOrDie(message: E => String): Handler.Id[L, N] =
      getOrElse(e => sys.error(message(e)))

    def mapLeft[E2](f: E => E2): Handler[Either[E2, _], L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.swap.map(f).swap)
