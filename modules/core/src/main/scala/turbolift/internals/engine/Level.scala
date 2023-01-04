package turbolift.internals.engine
import turbolift.internals.interpreter.Void


private[engine] final class Level(
  val prompt: Prompt,
  val segments: List[Segment],
  val savedLookup: Lookup,
):
  def copy(segments: List[Segment] = segments): Level = new Level(prompt, segments, savedLookup)

  export segments.{isEmpty, nonEmpty}

  override def toString = s"${prompt.toStr}[${segments.mkString(" > ")}]"

  
  def maxHeight: Int = segments.head.frame.height

  def top: Segment = segments.head

  def drop: Level = copy(segments = segments.tail)
