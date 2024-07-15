package turbolift.bindless
import scala.annotation.tailrec
import scala.quoted.*
import cps.{CpsMonad, CpsMonadContext, CpsMonadConversion}
import turbolift.Computation


transparent inline def `do`[A](inline body: A) = ${ Macro.impl[A]('body) }


extension [A, U](comp: Computation[A, U])
  inline def ! : A = compiletime.error("Invalid outside `do` block.")


private object Macro:
  def impl[A: Type](body: Expr[A])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val effect: TypeRepr = joinEffects(flattenEffects(collectEffects(body.asTerm)))
    effect.asType match
      case '[u] =>
        val transformedBody = transform(body.asTerm):
          case Apply(TypeApply(Ident("!"), List(a, u2)), List(comp)) =>
            (a.tpe.asType, u2.tpe.asType) match
              case ('[a], '[u2]) =>
                '{
                  cps.await[Computation[_, u2], a, Computation[_, u2]](${comp.asExprOf[Computation[a, u2]]})(
                    using monadAndContext.as[u2], CpsMonadConversion.identityConversion
                  )
                }.asTerm
        '{
          given CpsMonad[Computation[_, u]] = monadAndContext.as[u]
          cps.async {
            ${transformedBody.asExprOf[A]}
          }.asInstanceOf[Computation[A, u]]
        }


  private def collectEffects(using Quotes)(body: quotes.reflect.Term): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    var effects = List.empty[TypeRepr]
    traverse(body):
      case Apply(TypeApply(Ident("!"), List(_, x)), List(_)) => effects ::= x.tpe
    effects


  private def flattenEffects(using Quotes)(l: List[quotes.reflect.TypeRepr]): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    @tailrec def loop(l: List[TypeRepr], acc: List[TypeRepr]): List[TypeRepr] =
      l match
        case Nil => acc.distinct
        case AndType(a, b) :: Nil => loop(a :: b :: Nil, acc)
        case head :: tail => loop(tail, head :: acc)
    loop(l, Nil)


  private def joinEffects(using Quotes)(l: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    l match
      case Nil => TypeRepr.of[Any]
      case effects => effects.reduce((a, b) => AndType(a, b))


  private def traverse(using Quotes)(tree: quotes.reflect.Tree)(pf: PartialFunction[quotes.reflect.Tree, Unit]): Unit =
    import quotes.reflect.*
    new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        pf.lift(tree).getOrElse(super.traverseTree(tree)(owner))
    .traverseTree(tree)(Symbol.spliceOwner)


  private def transform(using Quotes)(tree: quotes.reflect.Tree)(pf: PartialFunction[quotes.reflect.Tree, quotes.reflect.Term]): quotes.reflect.Tree =
    import quotes.reflect.*
    new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        pf.lift(tree).getOrElse(super.transformTerm(tree)(owner))
    .transformTree(tree)(Symbol.spliceOwner)


  val monadAndContext: MonadAndContext[Any] = new MonadAndContext[Any]


  final class MonadAndContext[U] extends CpsMonadContext[Computation[_, U]] with CpsMonad[Computation[_, U]]:
    type F[A] = Computation[A, U]
    override type Context = MonadAndContext[U]
    override def monad: CpsMonad[F] = this
    override def apply[A](op: Context => F[A]): F[A] = op(this)
    override def pure[T](t: T): F[T] = Computation.pure(t)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = fa.map(f)
    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = fa.flatMap(f)
    def as[U2]: MonadAndContext[U2] = asInstanceOf[MonadAndContext[U2]]
