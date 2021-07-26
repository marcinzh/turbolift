package turbolift.abstraction.internals.engine
import cats.{Id, ~>}
import turbolift.abstraction.{!!, ComputationCases, HandlerCases}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.internals.effect.EffectId
import turbolift.abstraction.internals.interpreter.{AnySignature, MonadTransformer}
import turbolift.std_effects.ChoiceSig
import turbolift.abstraction.internals.interpreter.InterpreterCases


sealed trait Item[M[_], U] { val effectId: EffectId }
case class Trans[M[_], U](override val effectId: EffectId, transStack: TransformerStack[M]) extends Item[M, U]
case class Proxy[M[_], U](override val effectId: EffectId, sig: AnySignature[U]) extends Item[M, U]


final class MainLoop[M[_], U](
  val theMonad: MonadPar[M],
  val effStack: Vector[Item[M, U]],
):
  def run[A](ua: A !! U): M[A] = loop(ua, Step.empty)

  private def loop[A, B](ua: A !! U, step: Step[A, B, M, U]): M[B] =
    import ComputationCases._
    import StepCases._

    ua match
      case Pure(a) => loop(Done(theMonad.pure(a)), step)
      case ua: Done[A, M, U] @unchecked =>
        // case Done(ua: M[A] @unchecked) =>
        // case Done(ma_) =>
        val ma: M[A] = ua.value//.asInstanceOf[M[A]]
        step match
          case Empty() => ma.asInstanceOf[M[B]] //@#@
          case SeqStep(f, next) => theMonad.flatMap(ma)(a => loop(f(a), next))
          case ParStepLeft(ux, next) => loop(ux, ParStepRight(ma, next))
          case ParStepRight(mx, next) => loop(Done(theMonad.zipPar(mx, ma)), next)
        
      case Defer(th) => theMonad.defer(loop(th(), step))
      case FlatMap(ux, k) => loop(ux, SeqStep(k, step))
      case ZipPar(ux, uy) => loop(ux, ParStepLeft(uy, step))

      case Impure(id, op) =>
        val sig = lookup(id)
        val resp = op.asInstanceOf[AnySignature[U] => A !! U].apply(sig)
        loop(resp, step)

      case Delimit(ux, h) => h.interpreter match
        case interp: InterpreterCases.SaturatedTrans =>
          val mx = interp.prime(pushTrans(interp.effectIds, interp.transformer).run(ux))
          loop(Done(mx), step)
        case proxy: InterpreterCases.Proxy[?] =>
          val mx = pushProxy(proxy).run(ux.asInstanceOf[A !! U])
          loop(Done(mx), step)


  def pushTrans[T[_[_], _], O[_], V](effectIds: Vector[EffectId], transformer: MonadTransformer[T, O]): MainLoop[T[M, _], U with V] =
    val newTransStack = TransformerStack.pushFirst(transformer)(this.theMonad)
    val newHeads = effectIds.map(Trans[T[M, _], U with V](_, newTransStack))
    val newTail: Vector[Item[T[M, _], U with V]] = effStack.map {
      case Trans(id, st) => Trans[T[M, _], U with V](id, st.pushNext(transformer))
      case x => x.asInstanceOf[Item[T[M, _], U with V]]
    }
    val newEffStack = newTail ++ newHeads
    val newLoop = new MainLoop[T[M, _], U with V](newTransStack.outerMonad, newEffStack)
    newLoop.vmtTieKnots()
    newLoop.vmtMakeChoice()
    newLoop

  def pushProxy[V](proxy: InterpreterCases.Proxy[V]): MainLoop[M, U with V] =
    val sig: AnySignature[U with V] = proxy.onOperation[U with V]
    val newHeads = proxy.effectIds.map(Proxy[M, U with V](_, sig))
    val newTail: Vector[Item[M, U with V]] = effStack.map(_.asInstanceOf[Item[M, U with V]])
    val newEffStack = newTail ++ newHeads
    val newLoop = new MainLoop[M, U with V](theMonad, newEffStack)
    newLoop.vmtTieKnots()
    newLoop.vmtMakeChoice()
    newLoop

  private def lookup(id: EffectId): AnySignature[U] = vmtLookup(id).asInstanceOf[AnySignature[U]]

  //-------- low level stuff --------

  private val vmt: Array[AnyRef] = new Array[AnyRef]((effStack.size + 1) * 2) //// *2 for KV pair, +1 for Choice

  private def vmtLookup(key: AnyRef): AnyRef =
    def loop(i: Int): AnyRef =
      if vmt(i) eq key
      then vmt(i+1)
      else loop(i+2)
    loop(0)

  private def vmtTieKnots(): Unit =
    val n = effStack.size - 1
    0.to(n).foreach { i =>
      val (k, v) = effStack(n - i) match
        case Trans(k, v) => (k, v.decoder[U]([X] => (comp: X !! U) => run(comp)))
        case Proxy(k, v) => (k, v)
      vmt(i*2) = k
      vmt(i*2+1) = v
    }

  private def vmtMakeChoice(): Unit =
    val n = effStack.size * 2
    def loop(i: Int): Unit =
      if i < n then
        val sig = vmt(i+1)
        if sig.isInstanceOf[ChoiceSig[?]]
        then vmt(n + 1) = sig
        else loop(i + 2)
    loop(0)


object MainLoop:
  val pure: MainLoop[Trampoline, Any] = fromMonad(TrampolineInstances.monad)
  val pureStackUnsafe: MainLoop[Id, Any] = fromMonad(MonadPar[Id])
  def fromMonad[M[_]: MonadPar]: MainLoop[M, Any] = new MainLoop[M, Any](MonadPar[M], Vector())
