package turbolift.io
import turbolift.!!
import turbolift.effects.{CoroutineEffect, IO}
import turbolift.io.BlockingVar
import turbolift.Extensions._
import CoroutineEffect.Step
import CoroutineImpl.Status


trait Coroutine[-I, +O, +R, -U]:
  def resumeEff[U2 <: U](iEff: I !! U2): Either[R, O] !! (U2 & IO)
  final def resume(i: I): Either[R, O] !! (U & IO) = resumeEff(i.pure_!!)
  final def resume(using ev: Unit <:< I): Either[R, O] !! (U & IO) = resume(ev(()))
  final def tryResumeEff[U2 <: U](iEff: I !! U2): Option[O] !! (U2 & IO) = resumeEff(iEff).map(_.toOption)
  final def tryResume(i: I): Option[O] !! (U & IO) = resumeEff(i.pure_!!).map(_.toOption)
  final def tryResume(using ev: Unit <:< I): Option[O] !! (U & IO) = resume(ev(())).map(_.toOption)

  def result: Option[R] !! IO
  final def canResume: Boolean !! IO = result.map(!_.isDefined)


object Coroutine:
  def create[I, O, R, U](body: (fx: CoroutineEffect[I, O, R]) => I => R !! (U & fx.type)): Coroutine[I, O, R, U] !! IO =
    case object Fx extends CoroutineEffect[I, O, R]
    for
      avar <- BlockingVar.create[Status[I, R, U & Fx.type]](Right(body(Fx)))
      coro = new CoroutineImpl[I, O, R, U](Fx)(avar)
    yield coro

  @annotation.targetName("createUnit")
  def create[O, R, U](body: (fx: CoroutineEffect[Unit, O, R]) => R !! (U & fx.type)): Coroutine[Unit, O, R, U] !! IO =
    create[Unit, O, R, U](fx => _ => body(fx))


private final class CoroutineImpl[I, O, R, U](Fx: CoroutineEffect[I, O, R])(avar: BlockingVar[Status[I, R, U & Fx.type]]) extends Coroutine[I, O, R, U]:
  private type Fx = Fx.type
  private val handler = Fx.handler[U]

  override def resumeEff[U2 <: U](iEff: I !! U2): Either[R, O] !! (U2 & IO) =
    avar.updateEff:
      case s @ Left(r) => !!.pure((Left(r), s))
      case Right(k) => iEff.flatMap(k).handleWith(handler).map:
        case Step.Exit(r) => (Left(r), Left(r))
        case Step.Yield(o, k2) => (Right(o), Right(k2))

  override def result: Option[R] !! IO = avar.gets(_.fold(Some(_), _ => None))


private object CoroutineImpl:
  type Status[I, R, U] = Either[R, I => R !! U]
