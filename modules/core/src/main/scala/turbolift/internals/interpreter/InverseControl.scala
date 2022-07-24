package turbolift.internals.interpreter
import cats.Functor
import turbolift.!!
import turbolift.typeclass.MonadZip
import turbolift.ComputationCases.Lift


private[internals] sealed trait InverseControl:
  enclosing =>
  type UpperFunctor[+_]
  type UpperMonad[_]
  type LowerMonad[_]
  type FocusTrans[_[_], _]
  final type FocusMonad[X] = FocusTrans[LowerMonad, X]

  def withControl[A](ff: ThisControl => FocusMonad[UpperFunctor[A]]): UpperMonad[A]

  def upperFunctor: Functor[UpperFunctor]
  def lowerMonad: MonadZip[LowerMonad]

  final type WithControlArg[A] = ThisControl => FocusMonad[UpperFunctor[A]]

  final type ThisControl = Control[FocusTrans] {
    type UpperFunctor[+X] = enclosing.UpperFunctor[X]
    type UpperMonad[X] = enclosing.UpperMonad[X]
    type LowerMonad[X] = enclosing.LowerMonad[X]
  }

  trait ThisControlImpl extends Control[FocusTrans]:
    final override type UpperFunctor[+X] = enclosing.UpperFunctor[X]
    final override type UpperMonad[X] = enclosing.UpperMonad[X]
    final override type LowerMonad[X] = enclosing.LowerMonad[X]
    final override def upperFunctor: Functor[UpperFunctor] = enclosing.upperFunctor
    final override def lowerMonad: MonadZip[LowerMonad] = enclosing.lowerMonad

  type Layer[T[_[_], _], F[+_]] = InverseControl {
    type UpperFunctor[+X] = enclosing.UpperFunctor[F[X]]
    type UpperMonad[X] = T[enclosing.UpperMonad, X]
    type LowerMonad[X] = enclosing.LowerMonad[X]
    type FocusTrans[Y[_], X] = enclosing.FocusTrans[Y, X]
  }

  trait LayerImpl[T[_[_], _], F[+_]] extends InverseControl:
    final override type UpperFunctor[+X] = enclosing.UpperFunctor[F[X]]
    final override type UpperMonad[X] = T[enclosing.UpperMonad, X]
    final override type LowerMonad[X] = enclosing.LowerMonad[X]
    final override type FocusTrans[Y[_], X] = enclosing.FocusTrans[Y, X]
    final override def lowerMonad: MonadZip[LowerMonad] = enclosing.lowerMonad

  type Roof[U] = InverseControl {
    type UpperFunctor[+X] = enclosing.UpperFunctor[X]
    type UpperMonad[X] = X !! U
    type LowerMonad[X] = enclosing.LowerMonad[X]
    type FocusTrans[Y[_], X] = enclosing.FocusTrans[Y, X]
  }

  final def roof[U](recur: [X] => (X !! U) => UpperMonad[X]): Roof[U] =
    new InverseControl:
      override type UpperFunctor[+X] = enclosing.UpperFunctor[X]
      override type UpperMonad[X] = X !! U
      override type LowerMonad[X] = enclosing.LowerMonad[X]
      override type FocusTrans[Y[_], X] = enclosing.FocusTrans[Y, X]
      override def lowerMonad: MonadZip[LowerMonad] = enclosing.lowerMonad
      override def upperFunctor: Functor[UpperFunctor] = enclosing.upperFunctor

      override def withControl[A](ff: ThisControl => FocusMonad[UpperFunctor[A]]): A !! U =
        Lift(enclosing.withControl { kk =>
          ff(new ThisControlImpl:
            override def inner[A](a: A): this.UpperFunctor[A] = kk.inner(a)
            override def locally[A](body: A !! U): FocusMonad[this.UpperFunctor[A]] = kk.locally(recur(body))
          )
        })


private[internals] object InverseControl:
  type Focus[T[_[_], _], M[_]] = InverseControl {
    type UpperMonad[X] = T[M, X]
    type UpperFunctor[+X] = X
    type LowerMonad[X] = M[X]
    type FocusTrans[Y[_], X] = T[Y, X]
  }

  def focus[T[_[_], _], M[_]](M: MonadZip[M]): Focus[T, M] =
    new InverseControl:
      override type UpperMonad[X] = T[M, X]
      override type UpperFunctor[+X] = X
      override type LowerMonad[X] = M[X]
      override type FocusTrans[Y[_], X] = T[Y, X]
      override val lowerMonad: MonadZip[LowerMonad] = M
      override val upperFunctor: Functor[UpperFunctor] = Functor[[X] =>> X]

      override def withControl[A](ff: ThisControl => FocusMonad[A]): FocusMonad[A] = ff(constControl)
      private val constControl: ThisControlImpl = new:
        override def inner[A](a: A): A = a
        override def locally[A](body: FocusMonad[A]): FocusMonad[A] = body
