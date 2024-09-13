package turbolift.internals.engine.stacked


private[engine] object Location:
  final val MAX_SEGMENT_SIZE = 12


  final case class Shallow(promptIndex: Int, storeIndex: Int):
    def toStr: String =
      s"loc{pi=$promptIndex si=$storeIndex}"


  opaque type Deep = Entry

  object Deep:
    inline def apply(that: Entry): Deep = that
    extension (thiz: Deep)
      inline def promptIndex: Int = thiz.promptIndex
      inline def storeIndex: Int = thiz.storeIndex
      inline def segmentDepth: Int = thiz.segmentDepth

      def shallow: Shallow = Shallow(thiz.promptIndex, thiz.storeIndex)

      def toStr: String =
        s"loc{pi=$promptIndex si=$storeIndex d=${segmentDepth}"
