package turbolift.internals.engine
import scala.annotation.tailrec


private final class StoreNel(
  val head: StoreSegment,
  _tail: Store.Underlying,
):
  def tail: Store = Store.wrap(_tail)
  def asStore: Store = Store.wrap(this)
