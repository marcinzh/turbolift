package turbolift.internals.engine


private[engine] opaque type Mark = Mark.Underlying

private[engine] object Mark extends Mark_opaque:
  type Underlying = Prompt | Null

  inline def wrap(x: Underlying): Mark = x
  
  extension (thiz: Mark)
    inline def unwrap: Underlying = thiz
