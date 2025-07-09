package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.ChannelImpl


sealed trait Channel[A] extends Channel.Get[A] with Channel.Put[A]:
  final def asGet: Channel.Get[A] = this
  final def asPut: Channel.Put[A] = this


object Channel:
  type Untyped = Channel[Any]
  private[turbolift] trait Unsealed extends Channel[Any]


  def bounded[A](capacity: Int): Channel[A] !! IO = !!.impure(unsafeCreate(capacity))
  def synchronous[A]: Channel[A] !! IO = bounded(0)
  def unbounded[A]: Channel[A] !! IO = bounded(Int.MaxValue)
  def unsafeCreate[A](capacity: Int): Channel[A] = new ChannelImpl(capacity.max(0)).asInstanceOf[Channel[A]]
  def unsafeUnbounded[A]: Channel[A] = unsafeCreate(Int.MaxValue)


  final case class Status(size: Int, capacity: Int, isBlocking: Boolean):
    def room: Int = capacity - size
    def isEmpty: Boolean = size == 0
    def isFull: Boolean = room == 0
    def isOverflow: Boolean = isFull && isBlocking
    def isUnderflow: Boolean = isEmpty && isBlocking


  sealed trait Base:
    final def size: Int !! IO = !!.impure(unsafeStatus().size)
    final def room: Int !! IO = !!.impure(unsafeStatus().room)
    final def isEmpty: Boolean !! IO = !!.impure(unsafeStatus().isEmpty)
    final def isFull: Boolean !! IO = !!.impure(unsafeStatus().isFull)
    final def isOverflow: Boolean !! IO = !!.impure(unsafeStatus().isOverflow)
    final def isUnderflow: Boolean !! IO = !!.impure(unsafeStatus().isUnderflow)

    def unsafeStatus(): Status

    private[turbolift] final def asImpl: ChannelImpl = asInstanceOf[ChannelImpl]


  sealed trait Get[A] extends Base:
    final def get: A !! IO = CC.intrinsic(_.intrinsicGetChannel(this))
    final def tryGet: Option[A] !! IO = !!.impure(unsafeTryGet())

    def unsafeTryGet(): Option[A]


  sealed trait Put[A] extends Base:
    final def put(value: A): Unit !! IO = CC.intrinsic(_.intrinsicPutChannel(this, value))
    final def tryPut(value: A): Boolean !! IO = !!.impure(unsafeTryPut(value))

    def unsafeTryPut(value: A): Boolean
