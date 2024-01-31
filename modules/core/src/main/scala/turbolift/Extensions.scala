package turbolift
import turbolift.internals.extensions._

/** Orphaned extensions.
 *
 *  Extensions of standard library types, like `Iterable`, `Iterator`, etc.
 *
 *  Usage: just import them all.
 *  {{{
 *  import turbolift.Extensions._
 *  }}}
 */
object Extensions
  extends MiscExtensions
  with FoldExtensions
  with MapFilterExtensions
  with TraverseExtensions
