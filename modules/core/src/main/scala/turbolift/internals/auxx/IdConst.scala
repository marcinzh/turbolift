package turbolift.internals.auxx


object IdConst:
  type Identity[X] = X
  type Const[C] = [_] =>> C
