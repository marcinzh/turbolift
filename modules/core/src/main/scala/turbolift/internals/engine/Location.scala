package turbolift.internals.engine


private object Location:
  final val MAX_SEGMENT_SIZE = 12


  final case class Shallow(promptIndex: Int, storeIndex: Int):
    def deep(segmentDepth: Int): Deep = Deep(promptIndex, storeIndex, segmentDepth)

    def toStr: String =
      s"loc{pi=$promptIndex si=$storeIndex}"


  final case class Deep(promptIndex: Int, storeIndex: Int, segmentDepth: Int):
    def shallow: Shallow = Shallow(promptIndex, storeIndex)

    def toStr: String =
      s"loc{pi=$promptIndex si=$storeIndex d=${segmentDepth}"
