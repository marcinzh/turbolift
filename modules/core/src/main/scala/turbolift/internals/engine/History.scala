package turbolift.internals.engine
import turbolift.!!
import turbolift.internals.primitives.Tags
import HistoryCases._


private[engine] sealed abstract class History(val tag: Int):
  def isEmpty: Boolean = tag == Tags.History_Empty


private[engine] object HistoryCases:
  case object Empty extends History(Tags.History_Empty)

  final class Proxied(
    val step: Step,
    val next: History,
    val savedLookup: Lookup,
  ) extends History(Tags.History_Proxied)

  final class Configured(
    val step: Step,
    val next: History,
    val savedConfig: Config,
  ) extends History(Tags.History_Configured)

  //@#@TODO more
