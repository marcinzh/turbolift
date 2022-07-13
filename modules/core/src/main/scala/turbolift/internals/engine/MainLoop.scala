package turbolift.internals.engine
import turbolift.{!!, Signature, ComputationCases, HandlerCases}
import turbolift.typeclass.MonadZip
import turbolift.std_effects.ChoiceSig
import turbolift.internals.effect.AnyChoice
import turbolift.internals.interpreter.{InterpreterCases, InverseControl}


private[engine] final class MainLoop[M[_], U](theMonad: MonadZip[M], effectStack: EffectStack):
  def run[A](ua: A !! U): M[A] = loop(ua, Step.empty)

  private def loop[A, B](ua: A !! U, step: Step[A, B, M, U]): M[B] =
    import ComputationCases._
    import StepCases._

    ua match
      case Pure(a) => loop(Lift(theMonad.pure(a)), step)
      case ua: Lift[A, M, U] @unchecked =>
        // case Lift(ua: M[A] @unchecked) =>
        // case Lift(ma_) =>
        val ma: M[A] = ua.value//.asInstanceOf[M[A]]
        step match
          case Done() => ma.asInstanceOf[M[B]] //@#@
          case More(f, next) => theMonad.flatMap(ma)(a => loop(f(a), next))
          case ZipLeft(ux, next) => loop(ux, ZipRight(ma, next))
          case ZipRight(mx, next) => loop(Lift(theMonad.zip(mx, ma)), next)

      case FlatMap(ux, k) => loop(ux, More(k, step))
      case Zip(ux, uy) => loop(ux, ZipLeft(uy, step))

      case Perform(sig, op) =>
        val op2 = op.asInstanceOf[Signature => Any]
        val f = vmtLookup(sig).asInstanceOf[(Signature => Any) => A !! U]
        loop(f(op2), step)

      case Delimit(ux, h) => h.interpreter match
        case interpreter: InterpreterCases.Flow =>
          val effectStack2 =
            val head = EffectStackItem.Flow(interpreter, InverseControl.focus(theMonad))
            val tail = effectStack.map {
              case EffectStackItem.Flow(x, ic) => EffectStackItem.Flow(x, interpreter.layer(ic))
              case x => x
            }
            head +: tail
          val theMonad2 = interpreter.transform(theMonad)
          val mainLoop2 = MainLoop(theMonad2, effectStack2)
          val mx = mainLoop2.run(ux)
          val mx2 = interpreter.prime(h.initial.asInstanceOf[interpreter.Initial], mx)
          loop(Lift(mx2), step)

        case interpreter: InterpreterCases.Proxy =>
          val effectStack2 = EffectStackItem.Proxy(interpreter) +: effectStack
          val mainLoop2 = MainLoop(theMonad, effectStack2)
          val mx = mainLoop2.run(ux).asInstanceOf[M[A]]
          loop(Lift(mx), step)


  //-------- low level stuff --------

  private def vmtLookup(key: AnyRef): AnyRef =
    def loop(i: Int): AnyRef =
      if vmt(i) eq key
      then vmt(i+1)
      else loop(i+2)
    loop(0)

  private val vmt: Array[AnyRef] =
    val sigCount = effectStack.iterator.map(_.interpreter.signatures.size).sum
    val array = new Array[AnyRef]((sigCount + 1) * 2) //// *2 for KV pair, +1 for Choice
    array(array.size - 2) = AnyChoice
    var index = 0
    val recur: [A] => (A !! U) => M[A] = [A] => (ua: A !! U) => run(ua)

    for
      esi <- effectStack
      fun = esi match
        case EffectStackItem.Flow(ip, ic) =>
          val roof = ic.asInstanceOf[InverseControl { type UpperMonad[X] = M[X] }].roof[U](recur)
          (method: Signature => Any) => roof.withControl(method(ip).asInstanceOf[roof.WithControlArg[Any]]) 
        case EffectStackItem.Proxy(ip) => (method: Signature => Any) => method(ip)
      _ = {
        if esi.interpreter.isInstanceOf[ChoiceSig] then
          array(array.size - 1) = fun
      }
      sig <- esi.interpreter.signatures
    do
      array(index) = sig
      array(index + 1) = fun
      index = index + 2
    array


private[turbolift] object MainLoop:
  val pure: MainLoop[Trampoline, Any] = fromMonad(TrampolineInstances.monad)
  def fromMonad[M[_]: MonadZip]: MainLoop[M, Any] = new MainLoop[M, Any](MonadZip[M], Array())
