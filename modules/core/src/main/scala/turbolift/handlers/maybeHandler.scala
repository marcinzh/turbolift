package turbolift.handlers
import turbolift.!!
import turbolift.effects.{MaybeEffect, MaybeSignature}
import turbolift.Extensions._


extension (fx: MaybeEffect)
  def maybeHandler: fx.ThisHandler[Identity, Option, Any] =
    new fx.impl.Stateless[Identity, Option, Any] with fx.impl.Parallel with MaybeSignature:
      override def onReturn(a: Unknown): Option[Unknown] !! Any = !!.pure(Some(a))

      override def onRestart(aa: Option[Unknown]): Unknown !! fx.type =
        aa match
          case Some(a) => !!.pure(a)
          case _ => fx.empty

      override def onUnknown(aa: Option[Unknown]): Option[Unknown] = aa

      override def onZip[A, B, C](ea: Option[A], eb: Option[B], k: (A, B) => C): Option[C] =
        (ea, eb) match
          case (Some(a), Some(b)) => Some(k(a, b))
          case _ => None

      override def empty: Nothing !! ThisEffect = Control.abort(None)

      override def catchToOption[A, U <: ThisEffect](body: A !! U): Option[A] !! U = Control.delimit(body)

    .toHandler
