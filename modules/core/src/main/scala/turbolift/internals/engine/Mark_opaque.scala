package turbolift.internals.engine


private[engine] trait Mark_opaque:
  final def none: Mark = Mark.wrap(null)

  extension (thiz: Mark)
    final def isEmpty = thiz.unwrap == null
    final def nonEmpty = !thiz.isEmpty

    final def toStr: String = if thiz.isEmpty then "NOMARK" else s"@${thiz.unwrap}"

    final def mustBeEmpty(): Unit = if thiz.nonEmpty then despair
