package turbolift.internals.engine.stacked


private final class Entry(
  val prompt: Prompt,
  val promptIndex: Int,
  val storeIndex: Int,
):
  def this(prompt: Prompt, location: Location.Shallow) =
    this(prompt, promptIndex = location.promptIndex, storeIndex = location.storeIndex)

  def deepLocation(depth: Int): Location.Deep =
    Location.Deep(promptIndex = promptIndex, storeIndex = storeIndex, segmentDepth = depth)


private object Entry:
  val initial: Entry = Entry(Prompt.IO, promptIndex = 0, storeIndex = 0)
