package turbolift.internals.engine.stacked


private[engine] final class Entry(
  val prompt: Prompt,
  val promptIndex: Short,
  val storeIndex: Short,
  val segmentDepth: Int, //// Initially meaningless
):
  def this(prompt: Prompt, promptIndex: Int, storeIndex: Int) =
    this(
      prompt,
      promptIndex = promptIndex.toShort,
      storeIndex = storeIndex.toShort,
      segmentDepth = 0,
    )

  def this(prompt: Prompt, location: Location.Shallow) =
    this(
      prompt,
      promptIndex = location.promptIndex,
      storeIndex = location.storeIndex,
    )

  inline def deepLocation(depth: Int): Location.Deep =
    if depth == 0 then
      Location.Deep(this)
    else
      Location.Deep(copyWithDepth(depth))

  def copyWithDepth(n: Int): Entry =
    new Entry(
      prompt = prompt,
      promptIndex = promptIndex,
      storeIndex = storeIndex,
      segmentDepth = n,
    )

  //@#@TEMP
  inline def location: Location.Deep = Location.Deep(this)


private object Entry:
  val initial: Entry = Entry(Prompt.IO, promptIndex = 0, storeIndex = 0, segmentDepth = 0)
