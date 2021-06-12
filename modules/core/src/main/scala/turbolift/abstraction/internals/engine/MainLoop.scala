package turbolift.abstraction.internals.engine
import cats.{Id, ~>}
import turbolift.abstraction.{!!, ComputationCases, HandlerCases}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.internals.effect.EffectId
import turbolift.abstraction.internals.interpreter.{AnySignature, MonadTransformer}
import turbolift.std_effects.ChoiceSig
import turbolift.abstraction.internals.interpreter.InterpreterCases


sealed trait Item[M[_], U]
case class Trans[M[_], U](transStack: TransformerStack[M]) extends Item[M, U]
case class NotTrans[M[_], U](id: EffectId, sig: AnySignature[U]) extends Item[M, U]


final class MainLoop[M[_], U](
  val theMonad: MonadPar[M],
  val effStack: Vector[Item[M, U]],
) extends ((* !! U) ~> M) {
  override def apply[A](ua: A !! U): M[A] = loop(ua, Step.empty)

  private def loop[A](ua: A !! U, step: Step[M, U]): M[A] = {
    import ComputationCases._
    import StepCases._
    def castS[Z[U1] <: AnySignature[U1]](f: Z[U] => A !! U) = f.asInstanceOf[AnySignature[U] => A !! U]
    def castU[A1](ua1: A1 !! U) = ua1.asInstanceOf[A !! U]
    def castM[A1](ma1: M[A1]) = ma1.asInstanceOf[M[A]]

    ua match {
      case Pure(a) => loop(Done(theMonad.pure(a)), step)
      case ua: Done[tA, tM, U] =>
        val ma: M[A] = ua.value.asInstanceOf[M[A]]
        step match {
          case Empty() => ma
          case SeqStep(f, next) => theMonad.flatMap(ma)(a => loop(castU(f(a)), next))
          case ParStepLeft(ux, next) => loop(castU(ux), ParStepRight(ma, next))
          case ParStepRight(mx, next) => loop(Done(castM(theMonad.zipPar(mx, ma))), next)
        }
      case Defer(th) => theMonad.defer(loop(th(), step))
      case FlatMap(ux, k) => loop(castU(ux), SeqStep(k, step))
      case ZipPar(ux, uy) => loop(castU(ux), ParStepLeft(castU(uy), step))
      case Impure(id, op) => loop(castS(op)(lookup(id)), step)
      case ua: Delimit[tA, tU, tF, tL, tN] =>
        val interpreter = ua.handler.interpreter
        val body = ua.body
        val stuff = interpreter match {
          case st: InterpreterCases.SaturatedTrans => st.prime(pushTrans(st.transformer).apply(body))
          // case proxy: InterpreterCases.Proxy[U] => pushProxy(proxy).apply(body)
          case _ =>
            val proxy = interpreter.asInstanceOf[InterpreterCases.Proxy[U with tL]]
            pushProxy(proxy).apply(body)
        }
        loop(Done(stuff), step)
    }
  }

  def pushTrans[T[_[_], _], O[_], V](transformer: MonadTransformer[T, O]): MainLoop[T[M, *], U with V] = {
    val newTransStack = TransformerStack.pushFirst(transformer)(this.theMonad)
    val newHead = Trans[T[M, *], U with V](newTransStack)
    val newTail: Vector[Item[T[M, *], U with V]] = effStack.map {
      case Trans(st) => Trans[T[M, *], U with V](st.pushNext(transformer))
      case x => x.asInstanceOf[Item[T[M, *], U with V]]
    }
    val newEffStack = newTail :+ newHead
    val newLoop = new MainLoop[T[M, *], U with V](newTransStack.outerMonad, newEffStack)
    newLoop.vmtTieKnots()
    newLoop.vmtMakeChoice()
    newLoop
  }

  def pushProxy[V](proxy: InterpreterCases.Proxy[V]): MainLoop[M, U with V] = {
    val sig: AnySignature[U with V] = proxy.onOperation[U with V]
    val id: EffectId = proxy.effectId
    val newHead = NotTrans[M, U with V](id, sig)
    val newTail: Vector[Item[M, U with V]] = effStack.map(_.asInstanceOf[Item[M, U with V]])
    val newEffStack = newTail :+ newHead
    val newLoop = new MainLoop[M, U with V](theMonad, newEffStack)
    newLoop.vmtTieKnots()
    newLoop.vmtMakeChoice()
    newLoop
  }


  private def lookup(id: EffectId): AnySignature[U] = vmtLookup(id).asInstanceOf[AnySignature[U]]

  //-------- low level stuff --------

  private val vmt: Array[AnyRef] = new Array[AnyRef]((effStack.size + 1) * 2) //// *2 for KV pair, +1 for Choice

  private def vmtLookup(key: AnyRef): AnyRef = {
    def loop(i: Int): AnyRef = {
      if (vmt(i) eq key)
        vmt(i+1)
      else
        loop(i+2)
    }
    loop(0)
  }

  private def vmtTieKnots(): Unit = {
    val n = effStack.size - 1
    0.to(n).foreach { i =>
      val (k, v) = effStack(n - i) match {
        case Trans(st) => (st.effectId, st.decoder(this))
        case NotTrans(k, v) => (k, v)
      }
      vmt(i*2) = k
      vmt(i*2+1) = v
    }
  }

  private def vmtMakeChoice(): Unit = {
    val n = effStack.size * 2
    def loop(i: Int): Unit = {
      if (i < n) {
        val sig = vmt(i+1)
        if (sig.isInstanceOf[ChoiceSig[U]])
          vmt(n + 1) = sig
        else
          loop(i + 2)
      }
    }
    loop(0)
  }
}


object MainLoop {
  val pure: MainLoop[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: MainLoop[Id, Any] = apply(MonadPar.identity)
  def apply[M[_]: MonadPar]: MainLoop[M, Any] = new MainLoop[M, Any](MonadPar[M], Vector())
}
