package turbolift.typeclass


trait ExtendTuple[A, B, C]:
  def extendTuple(tuple: A, value: B): C

private sealed trait ExtendTupleLow:
  given [A, B]: ExtendTuple[A, B, (A, B)] with
    override def extendTuple(a: A, b: B) = (a, b)

object ExtendTuple extends ExtendTupleLow:
  given [A, B, X]: ExtendTuple[(A, B), X, (A, B, X)] with
    override def extendTuple(tuple: (A, B), x: X) =
      val (a, b) = tuple
      (a, b, x)

  given [A, B, C, X]: ExtendTuple[(A, B, C), X, (A, B, C, X)] with
    override def extendTuple(tuple: (A, B, C), x: X) =
      val (a, b, c) = tuple
      (a, b, c, x)

  given [A, B, C, D, X]: ExtendTuple[(A, B, C, D), X, (A, B, C, D, X)] with
    override def extendTuple(tuple: (A, B, C, D), x: X) =
      val (a, b, c, d) = tuple
      (a, b, c, d, x)

  given [A, B, C, D, E, X]: ExtendTuple[(A, B, C, D, E), X, (A, B, C, D, E, X)] with
    override def extendTuple(tuple: (A, B, C, D, E), x: X) =
      val (a, b, c, d, e) = tuple
      (a, b, c, d, e, x)

  given [A, B, C, D, E, F, X]: ExtendTuple[(A, B, C, D, E, F), X, (A, B, C, D, E, F, X)] with
    override def extendTuple(tuple: (A, B, C, D, E, F), x: X) =
      val (a, b, c, d, e, f) = tuple
      (a, b, c, d, e, f, x)

  given [A, B, C, D, E, F, G, X]: ExtendTuple[(A, B, C, D, E, F, G), X, (A, B, C, D, E, F, G, X)] with
    override def extendTuple(tuple: (A, B, C, D, E, F, G), x: X) =
      val (a, b, c, d, e, f, g) = tuple
      (a, b, c, d, e, f, g, x)
