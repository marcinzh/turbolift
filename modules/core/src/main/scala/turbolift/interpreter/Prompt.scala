package turbolift.interpreter

//// cant make `opaque` bcoz inline problems
type Prompt = Interpreter.Untyped

object Prompt:
  export Interpreter.Io.{prompt => IO}
