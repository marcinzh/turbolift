package turbolift.internals.interpreter
import cats.Functor
import turbolift.!!
import turbolift.typeclass.MonadZip

/** Interface to manipulate Turbolift's internal monad transformer stack.
  * 
  * Used by implementations of [[Interpreter.Flow Flow Interpreters]]. 
  */
trait Control[T[_[_], _]]:
  type UpperFunctor[+_]
  type UpperMonad[_]
  type LowerMonad[_]

  def upperFunctor: Functor[UpperFunctor]
  def lowerMonad: MonadZip[LowerMonad]

  def inner[A](a: A): UpperFunctor[A]
  final def inner(): UpperFunctor[Unit] = inner(())
  final def outer[A](a: A): LowerMonad[A] = lowerMonad.pure(a)
  def locally[A](body: UpperMonad[A]): T[LowerMonad, UpperFunctor[A]]


object Control:
  type Apply[T[_[_], _], U] = Control[T] { type UpperMonad[X] = X !! U }

  given [T[_[_], _]](using C: Control[T]): MonadZip[C.LowerMonad] = C.lowerMonad
  given [T[_[_], _]](using C: Control[T]): Functor[C.UpperFunctor] = C.upperFunctor
