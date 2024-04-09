package turbolift.internals.engine
import turbolift.interpreter.Interpreter


private final class Cache:
  var location: Location.Deep = Location.Deep.empty
  var interpreter: Interpreter.Untyped = null.asInstanceOf[Interpreter.Untyped]
  //@#@TODO obsolete prompt
  var prompt: Prompt = null.asInstanceOf[Prompt]
  def isEmpty = location.isEmpty

  def clear(): Unit =
    location = Location.Deep.empty
    interpreter = null.asInstanceOf[Interpreter.Untyped]
    prompt = null.asInstanceOf[Prompt]



private object Cache:
  val empty = new Cache()
