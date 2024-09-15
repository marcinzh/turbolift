package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Prompt


private[engine] opaque type ShadowMap = ShadowMap.Underlying


private[engine] object ShadowMap:
  type Underlying = Map[Signature, Int]

  def empty: ShadowMap = Map()

  extension (thiz: ShadowMap)
    inline def isEmpty: Boolean = thiz.isEmpty

    inline def get(sig: Signature): Int = thiz.applyOrElse(sig, default)

    inline def get(prompt: Prompt): Int = thiz.applyOrElse(prompt.signatures.head, default)

    def push(prompt: Prompt): ShadowMap =
      prompt.signatures.foldLeft(thiz): (m, sig) =>
        m.updatedWith(sig):
          case Some(n) => Some(n + 1)
          case None => Some(1)


  private val default: Signature => Int = _ => 0
