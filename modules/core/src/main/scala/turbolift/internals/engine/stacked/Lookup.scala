package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.Signature


private opaque type Lookup <: AnyRef = Array[Lookup.Element]


private object Lookup:
  type Element = Signature | Entry

  val empty: Lookup = Array[Element]()

  def blank(sigCount: Int): Lookup = new Array[Element](sigCount * 2)


  extension (thiz: Lookup)
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


    def findBySignatureWithShadow(sig: Signature, shadowCount: Int): Entry | Int =
      val n = thiz.size
      @tailrec def loop(i: Int, c: Int): Entry | Int =
        if i < n then
          if thiz(i) eq sig then
            if c == 0 then
              thiz(i + 1).asInstanceOf[Entry]
            else
              loop(i + 2, c - 1)
          else
            loop(i + 2, c)
        else
          c
      loop(0, shadowCount)


    def findByPrompt(prompt: Prompt): Entry | Null =
      val n = thiz.size
      val sig = prompt.signatures.head
      @tailrec def loop(i: Int): Entry | Null =
        if i < n then
          if thiz(i) eq sig then
            val entry = thiz(i + 1).asInstanceOf[Entry]
            if entry.prompt eq prompt then
              entry
            else
              loop(i + 2)
          else
            loop(i + 2)
        else
          null
      loop(0)


    def findByPromptWithShadow(prompt: Prompt, shadowCount: Int): Entry | Int =
      val n = thiz.size
      val sig = prompt.signatures.head
      @tailrec def loop(i: Int, c: Int): Entry | Int =
        if i < n then
          if thiz(i) eq sig then
            if c == 0 then
              val entry = thiz(i + 1).asInstanceOf[Entry]
              if entry.prompt eq prompt then
                entry
              else
                loop(i + 2, 0)
            else
              loop(i + 2, c - 1)
          else
            loop(i + 2, c)
        else
          c
      loop(0, shadowCount)


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
