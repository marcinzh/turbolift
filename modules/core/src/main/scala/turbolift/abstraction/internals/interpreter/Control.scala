package turbolift.abstraction.internals.interpreter
import cats.Functor
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar


trait Control[T[_[_], _]]:
  type UpperFunctor[+_]
  type UpperMonad[_]
  type LowerMonad[_]

  def upperFunctor: Functor[UpperFunctor]
  def lowerMonad: MonadPar[LowerMonad]

  def inner[A](a: A): UpperFunctor[A]
  final def inner(): UpperFunctor[Unit] = inner(())
  final def outer[A](a: A): LowerMonad[A] = lowerMonad.pure(a)
  def locally[A](body: UpperMonad[A]): T[LowerMonad, UpperFunctor[A]]


type Control_!![T[_[_], _], U] = Control[T] { type UpperMonad[X] = X !! U }


object Control:
  given [T[_[_], _]](using C: Control[T]): MonadPar[C.LowerMonad] = C.lowerMonad
  given [T[_[_], _]](using C: Control[T]): Functor[C.UpperFunctor] = C.upperFunctor
