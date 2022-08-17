package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.{!!, Signature}
import turbolift.internals.effect.AnyChoice


private[engine] opaque type Lookup = Array[LookupElem]

private type LookupElem = Prompt | Signature

private[engine] object Lookup:
  val empty: Lookup = Array[LookupElem](AnyChoice, Prompt.global)

  extension (thiz: Lookup)
    def top: Prompt = thiz(1).asInstanceOf[Prompt]

    def find(sig: Signature): Prompt =
      @tailrec def loop(i: Int): Prompt =
        if thiz(i) eq sig then
          thiz(i + 1).asInstanceOf[Prompt]
        else
          loop(i + 2)
      loop(0)

    private def contains(sig: Signature): Boolean =
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

    def push(p: Prompt): Lookup =
      val sigs = p.anyInterpreter.signatures
      val n = thiz.size
      val d = sigs.size * 2

      if sigs.exists(contains(_)) then
        val bads = sigs.filter(contains(_))
        throw new Panic(s"Effect shadowing not implemented. Shadowed effects = [${bads.mkString(", ")}]")

      val that = new Array[LookupElem](n + d)
      java.lang.System.arraycopy(thiz, 0, that, d, n)

      @tailrec def loop(i: Int, j: Int): Unit =
        if i < d then
          that(i) = sigs(j)
          that(i + 1) = p
          loop(i + 2, j + 1)
      loop(0, 0)

      if p.isChoice then
        that(n + d - 1) = p
      that
