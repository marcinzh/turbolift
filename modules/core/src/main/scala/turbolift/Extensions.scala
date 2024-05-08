package turbolift
import turbolift.internals.extensions._
import turbolift.internals.auxx.IdConst

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
  with TraverseExtensions:
    export IdConst._
