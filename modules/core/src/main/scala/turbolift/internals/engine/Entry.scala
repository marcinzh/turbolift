package turbolift.internals.engine


private final class Entry (
  val prompt: Prompt,
  val location: Location.Shallow,
)

object Entry:
  val initial: Entry = Entry(PromptIO, Location.Shallow(0, 0))
