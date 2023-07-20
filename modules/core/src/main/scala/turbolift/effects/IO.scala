package turbolift.effects
import turbolift.!!
import turbolift.internals.primitives.Primitives


/** Effect that indicates lack of guarantee of determinism.
  *
  * Running computation that requests `NonDet` effect,
  * may produce different results each time.
  *
  * This is a psedo-effect.
  * It functions as a type-level label only.
  * It cannot be interpreted by the user.
  */
sealed trait NonDet

/** Effect that indicates lack of guarantee of totality.
  *
  * Running computation that requests `NonTot` effect,
  * may complete without producing a result.
  *
  * This is a psedo-effect.
  * It functions as a type-level label only.
  * It cannot be interpreted by the user.
  */
sealed trait NonTot


/** Effect that indicates lack of guarantee of totality or determinism.
  *
  * This is a psedo-effect.
  * It functions as a type-level label only.
  * It cannot be interpreted by the user.
  */
sealed trait IO extends NonDet with NonTot



object IO:
  def apply[A](value: => A): A !! IO = Primitives.impure(() => value)

  def blocking[A](value: => A): A !! IO =
    //@#@TODO
    apply(value)

