package turbolift.internals.engine
import scala.reflect.ClassTag


private[engine] object Location:
  opaque type Shallow = Short
  opaque type Deep = Int

  final val MAX_SEGMENT_SIZE = 13


  object Shallow:
    def apply(promptIndex: Int, stanIndex: Int, isStateful: Boolean): Shallow =
      def isSmall(n: Int) = 0 <= n && n <= 0xF   
      assert(isSmall(promptIndex))
      assert(isSmall(stanIndex))
      val h = if isStateful then 1 else 0
      (stanIndex + (promptIndex << 4) + (h << 8)).toShort

    given ClassTag[Shallow] = shallowCT

    extension (thiz: Shallow)
      def stanIndex: Int = A(thiz)
      def promptIndex: Int = B(thiz)
      def isStateful: Boolean = C(thiz)
      def isStateless: Boolean = !isStateful
      def asDeep: Deep = thiz
      def withDepth(n: Int): Deep = thiz + (n << 9)

      def toStr: String =
        s"loc{pi=$promptIndex si=$stanIndex s=${if isStateful then 1 else 0}}"


  object Deep:
    def empty: Deep = -1

    given ClassTag[Deep] = deepCT

    extension (thiz: Deep)
      def stanIndex: Int = A(thiz)
      def promptIndex: Int = B(thiz)
      def isStateful: Boolean = C(thiz)
      def isStateless: Boolean = !isStateful
      def segmentDepth: Int = thiz >>> 9
      def isEmpty: Boolean = thiz < 0
      def nonEmpty: Boolean = !thiz.isEmpty
      def asShallow: Shallow = ABC(thiz).toShort
      def head: Deep = ABC(thiz)

      def toStr: String =
        val s = if isEmpty then "empty" else s"pi=$promptIndex si=$stanIndex s=${if isStateful then 1 else 0} d=${segmentDepth}"
        s"loc{$s}"

  private def A(n: Int): Int     = n & 0xF
  private def B(n: Int): Int     = (n >>> 4) & 0xF
  private def C(n: Int): Boolean = (n & 0x100) != 0
  private def ABC(n: Int): Int   = n & 0x1FF

  private def shallowCT = summon[ClassTag[Short]]
  private def deepCT = summon[ClassTag[Int]]
