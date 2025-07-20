package turbolift.type_safety
import turbolift.{!!, Handler}
import turbolift.effects.{ReaderEffect, WriterEffect, StateEffect}
import turbolift.Extensions._


object Dummies {
  case object Fx1 extends StateEffect[Double]
  case object Fx2 extends WriterEffect[String]
  case object Fx3 extends ReaderEffect[Boolean]
  type Fx1 = Fx1.type
  type Fx2 = Fx2.type
  type Fx3 = Fx3.type

  val h1 = Fx1.handler(0.0)
  val h2 = Fx2.handler
  val h3 = Fx3.handler(true)

  type H1 = h1.type
  type H2 = h2.type
  type H3 = h3.type

  type H12 = Handler[Identity, [X] =>> (Double, (String, X)), Fx1 & Fx2, Any]
  type H21 = Handler[Identity, [X] =>> (String, (Double, X)), Fx1 & Fx2, Any]
  type H321 = Handler[Identity, [X] =>> (String, (Double, X)), Fx1 & Fx2 & Fx3, Any]

  class Whatever
  def any[T] : T = ???

  type Eff1 = Whatever !! Fx1
  type Eff2 = Whatever !! Fx2 
  type Eff3 = Whatever !! Fx3
  type Eff12 = Whatever !! (Fx1 & Fx2)
  type Eff23 = Whatever !! (Fx2 & Fx3)
  type Eff123 = Whatever !! (Fx1 & Fx2 & Fx3)
}
