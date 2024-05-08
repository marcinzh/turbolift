package turbolift.handlers
import turbolift.!!
import turbolift.effects.{StateEffect, StateSignature}
import turbolift.Extensions._


extension [S](fx: StateEffect[S])
  def stateHandler_local(initial: S): fx.ThisHandler[Identity, (_, S), Any] =
    new fx.impl.Stateful[Identity, (_, S), Any] with fx.impl.Sequential.Restartable with StateSignature[S]:
      override type Stan = S

      override def tailResumptiveHint: Boolean = true

      override def onInitial: S !! Any = !!.pure(initial)

      override def onReturn(a: Unknown, s: S): (Unknown, S) !! Any = !!.pure((a, s))

      override def onRestart(a_s: (Unknown, S)): Unknown !! ThisEffect = fx.put(a_s._2) &&! !!.pure(a_s._1)

      override val get: S !@! ThisEffect = (k, s) => k(s)

      override def gets[A](f: S => A): A !@! ThisEffect = (k, s) => k(f(s))

      override def put(s2: S): Unit !@! ThisEffect = (k, s) => k((), s2)

      override def swap(s2: S): S !@! ThisEffect = (k, s) => k(s, s2)

      override def modify(f: S => S): Unit !@! ThisEffect = (k, s) => k((), f(s))

      override def modifyGet(f: S => S): S !@! ThisEffect =
        (k, s) =>
          val s2 = f(s)
          k(s2, s2)

      override def getModify(f: S => S): S !@! ThisEffect = (k, s) => k(s, f(s))

      override def getModifyGet(f: S => S): (S, S) !@! ThisEffect =
        (k, s) =>
          val s2 = f(s)
          k((s, s2), s2)

      override def update[A](f: S => (A, S)): A !@! ThisEffect =
        (k, s) =>
          val (a, s2) = f(s)
          k(a, s2)

      override def updateGet[A](f: S => (A, S)): (A, S) !@! ThisEffect =
        (k, s) =>
          val a_s @ (_, s2) = f(s)
          k(a_s, s2)

      override def getUpdate[A](f: S => (A, S)): (A, S) !@! ThisEffect =
        (k, s) =>
          val (a, s2) = f(s)
          k((a, s), s2)

      override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !@! ThisEffect =
        (k, s) =>
          val (a, s2) = f(s)
          k((a, s, s2), s2)

    .toHandler
