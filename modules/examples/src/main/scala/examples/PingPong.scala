package examples
import turbolift.{!!, Signature, Effect}
import turbolift.Extensions._
import turbolift.effects.{State, Console}


case object PingPong extends Example:
  override def description: String = """
    Two mutually dependent effects.
    Example adapted from "Handling Bidirectional Control Flow" paper.
  """

  //----- Effects -----

  trait PingSignature extends Signature:
    def ping: Unit !! (ThisEffect & Pong)

  trait PongSignature extends Signature:
    def pong: Unit !! (ThisEffect & Ping)

  case object Ping extends Effect[PingSignature] with PingSignature:
    override def ping = perform(_.ping)

  case object Pong extends Effect[PongSignature] with PongSignature:
    override def pong = perform(_.pong)

  type Ping = Ping.type

  type Pong = Pong.type

  //----- Handlers -----

  extension (fx: Ping)
    def ponger =
      new fx.impl.Proxy[Console] with PingSignature:
        override def ping: Unit !! (ThisEffect & Pong) =
          Console.println(s"${Console.CYAN}ping${Console.RESET}") &&!
          Pong.pong
      .toHandler

  extension (fx: Pong)
    def pinger(limit: Int) =
      case object S extends State[Int]
      new fx.impl.Proxy[Console & S.type] with PongSignature:
        override def pong: Unit !! (ThisEffect & Ping) =
          for
            i <- S.modifyGet(_ + 1)
            _ <- Console.println(s"${Console.MAGENTA}pong $i${Console.RESET}")
            _ <- 
              if i == limit
              then !!.unit
              else Ping.ping
          yield ()
      .toHandler
      .partiallyProvideWith[Console](S.handler(0).dropState)

  //----- Run -----

  override def apply(): Unit =
    Ping.ping
    .handleWith(Pong.pinger(10))
    .handleWith(Ping.ponger)
    .handleWith(Console.handler)
    .unsafeRun
