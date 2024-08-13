package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.Signature
import turbolift.internals.engine.Misc._


private opaque type Lookup <: AnyRef = Array[Lookup.Element]


private object Lookup:
  type Element = Signature | Entry

  val empty: Lookup = Array[Element]()

  def blank(sigCount: Int): Lookup = new Array[Element](sigCount * 2)


  extension (thiz: Lookup)
    def findBySignature(sig: Signature): Entry | Null =
      val n = thiz.size
      @tailrec def loop(i: Int): Entry | Null =
        if thiz(i) eq sig then
          thiz(i + 1).asInstanceOf[Entry]
        else
          if i + 2 < n then
            loop(i + 2)
          else
            null
      loop(0)


    def containsSignature(sig: Signature): Boolean =
      val n = thiz.size
      @tailrec def loop(i: Int): Boolean =
        if i < n then
          if thiz(i) eq sig then
            true
          else
            loop(i + 2)
        else
          false
      loop(0)


    def push(e: Entry): Lookup =
      val sigs = e.prompt.signatures
      val n = thiz.size
      val d = sigs.size * 2
      val that = new Array[Element](n + d)
      java.lang.System.arraycopy(thiz, 0, that, d, n)
      @tailrec def loop(i: Int, j: Int): Unit =
        if i < d then
          that(i) = sigs(j)
          that(i + 1) = e
          loop(i + 2, j + 1)
      loop(0, 0)
      that


    def pop: Lookup =
      val d = top.prompt.signatures.size * 2
      val n = thiz.size
      val that = new Array[Element](n - d)
      java.lang.System.arraycopy(thiz, d, that, 0, n - d)
      that


    def top: Entry = thiz(1).asInstanceOf[Entry]


    def startIndexForSetInPlace(destIdx: Int, sigCount: Int): Int = thiz.size - 2 * (destIdx + sigCount)
    def setInPlace(start: Int, i: Int, sig: Signature, e: Entry): Unit =
      thiz(start + 2 * i) = sig
      thiz(start + 2 * i + 1) = e

    def toStr =
      val ps = thiz.iterator.zipWithIndex.collect { case (e, i) if i % 2 == 1 => e.asInstanceOf[Entry].prompt }
      s"Lookup[${ps.mkString(", ")}]"


  def sigNotFound(s: Signature): Nothing = panic(s"Signature ${s} not found")
