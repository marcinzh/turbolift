package spot.internals
import turbolift.!!


opaque type Par[A, U] = !![A, U]

object Par:
  def wrap[A, U](aa: A !! U): Par[A, U] = aa
  def unwrap[A, U](aa: Par[A, U]): A !! U = aa
