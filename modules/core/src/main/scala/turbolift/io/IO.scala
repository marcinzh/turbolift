package turbolift.io
import turbolift.!!
import turbolift.internals.primitives.Primitives


sealed trait IO

object IO:
  def apply[A](value: => A): A !! IO = Primitives.impure(() => value)

  def blocking[A](value: => A): A !! IO =
    //@#@TODO
    apply(value)

