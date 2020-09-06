package turbolift.type_safety
import cats.implicits._
import turbolift.abstraction.{!!, Handler, IHandler}
import turbolift.std_effects.{Reader, Writer, State}


object Dummies {
  case object Fx1 extends State[Double]
  case object Fx2 extends Writer[String]
  case object Fx3 extends Reader[Boolean]

  val h1 = Fx1.handler(0.0)
  val h2 = Fx2.handler
  val h3 = Fx3.handler(true)

  type H1 = h1.type
  type H2 = h2.type
  type H3 = h3.type

  type H12 = IHandler[Lambda[X => (Double, (String, X))], Fx1.type with Fx2.type]
  type H21 = IHandler[Lambda[X => (String, (Double, X))], Fx1.type with Fx2.type]
  type H321 = IHandler[Lambda[X => (String, (Double, X))], Fx1.type with Fx2.type with Fx3.type]

  class Whatever
  def any[T] : T = ???

  type Eff1 = Whatever !! Fx1.type
  type Eff2 = Whatever !! Fx2.type 
  type Eff3 = Whatever !! Fx3.type
  type Eff12 = Whatever !! Fx1.type with Fx2.type 
  type Eff23 = Whatever !! Fx2.type with Fx3.type
  type Eff123 = Whatever !! Fx1.type with Fx2.type with Fx3.type
}
