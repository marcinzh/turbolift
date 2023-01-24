package turbolift.internals.extensions

/** No need to use this trait directly, because it's inherited by [[turbolift.Extensions Extensions]] object. */
/*private[turbolift]*/ trait AllOrphanedExtensions
  extends orphaned.MiscExtensions
  with orphaned.FoldExtensions
  with orphaned.MapFilterExtensions
  with orphaned.TraverseExtensions
