package turbolift.abstraction.effect


trait Signature {
  type Op[+A]
}

trait FailSig extends Signature {
  def fail: Op[Nothing]
}

private[abstraction] object FailSig {
  val encodeFail = (sig: FailSig) => sig.fail
}
