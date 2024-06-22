package turbolift.internals.engine
import scala.reflect.ClassTag


private object Location:
  opaque type Shallow = Short
  opaque type Deep = Int

  final val MAX_SEGMENT_SIZE = 12


  object Shallow:
    def apply(promptIndex: Int, localIndex: Int): Shallow =
      def isSmall(n: Int) = 0 <= n && n <= 0xF   
      assert(isSmall(promptIndex))
      assert(isSmall(localIndex))
      (localIndex + (promptIndex << 4)).toShort

    given ClassTag[Shallow] = shallowCT

    extension (thiz: Shallow)
      def localIndex: Int = A(thiz)
      def promptIndex: Int = B(thiz)
      def asDeep: Deep = thiz
      def withDepth(n: Int): Deep = thiz + (n << 8)

      def toStr: String =
        s"loc{pi=$promptIndex si=$localIndex}"


  object Deep:
    def empty: Deep = -1

    given ClassTag[Deep] = deepCT

    extension (thiz: Deep)
      def localIndex: Int = A(thiz)
      def promptIndex: Int = B(thiz)
      def segmentDepth: Int = thiz >>> 8
      def isEmpty: Boolean = thiz < 0
      def nonEmpty: Boolean = !thiz.isEmpty
      def asShallow: Shallow = AB(thiz).toShort
      def head: Deep = AB(thiz)

      def toStr: String =
        val s = if isEmpty then "empty" else s"pi=$promptIndex si=$localIndex d=${segmentDepth}"
        s"loc{$s}"

  private def A(n: Int): Int  = n & 0xF
  private def B(n: Int): Int  = (n >>> 4) & 0xF
  private def AB(n: Int): Int = n & 0xFF

  private def shallowCT = summon[ClassTag[Short]]
  private def deepCT = summon[ClassTag[Int]]
