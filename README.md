:construction: :construction: Work In Progress :construction: :construction:

# Turbolift

A functional effect system. An alternative to Eff monad and MTL, combining advantages of both:

- Like Eff monad, Turbolift allows user to conveniently introduce and eliminate new effects to/from the computation, at any point, without knowledge of, or interference from preexisting effects.

- Like MTL, Turbolift supports higher order effects.

Other features:

- Boilerplate free interface. 

- Parallel composition of effects.

- Unique labelling/tagging of effects. Always on, by design, as opposed to optional, with modifier, e.g. in [Idris](http://docs.idris-lang.org/en/latest/effects/state.html#labelled-effects).

# Example
```scala
import turbolift.abstraction.!!
import turbolift.std_effects.{Reader, Writer, State}

object Main extends App {
  // Declare some effects:
  case object MyReader extends Reader[Int]
  case object MyState extends State[Int]
  case object MyExcept extends Except[String]

  // Create a monadic computation using those effects:
  val computation = for {
    a <- MyState.get
    b <- MyReader.ask
    c <- {
      if (b != 0) 
        !!.pure(a / b)
      else 
        MyExcept.raise(s"Tried to divide $a by zero")
    }
    _ <- MyState.put(c)
  } yield ()

  // Create a handler for the above computation, by composing
  // individual handlers of each requested effect:
  val handler = MyExcept.handler <<<! MyState.handler(100).exec <<<! MyReader.handler(3)

  // Execute the computation using the handler:
  val result = handler.run(computation)

  println(result) // prints "Right(33)"
}
```
