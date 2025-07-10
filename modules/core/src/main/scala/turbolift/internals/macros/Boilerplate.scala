package turbolift.internals.macros
import scala.annotation.experimental
import scala.collection.mutable.ListBuffer
import scala.quoted.*
import turbolift.{Signature => Sig, Effect}


//@#@TODO doesn't properly handle methods with type parameters
@experimental object Boilerplate:
  private inline def debug(x: Any) = ()
  // private def debug(x: Any) = println(s"[Bolierplate] $x")

  def macroImpl[Z <: Sig: Type](using Quotes): Expr[Effect[Z] & Z] = {
    import quotes.reflect.*
    given Printer[TypeRepr] = Printer.TypeReprStructure
    given Printer[Tree] = Printer.TreeStructure


    val sigType = TypeRepr.of[Z]
    val sigSymbol = sigType.typeSymbol
    debug(s"Z = ${sigType.show}")

    val accumDefDefs = new ListBuffer[DefDef]
    val effectSymbol = Symbol.newClass(
      /*parent =*/ Symbol.spliceOwner,
      name = Symbol.freshName(sigSymbol.name),
      parents = List(TypeRepr.of[Object], TypeRepr.of[Effect[Z]], sigType),
      decls = makeOverrides(_, sigSymbol = sigSymbol, sigType = sigType, accumDefDefs),
      selfType = None,
    )

    val clsDef = ClassDef(
      cls = effectSymbol,
      parents = List(TypeTree.of[Object], TypeTree.of[Effect[Z]], TypeTree.of[Z]),
      body = accumDefDefs.toList
    )

    val newInstance =
      Typed(
        Apply(
          Select(
            New(TypeIdent(effectSymbol)),
            effectSymbol.primaryConstructor
          ),
          Nil
        ),
        TypeTree.of[Effect[Z] & Z]
      )
    Block(List(clsDef), newInstance).asExprOf[Effect[Z] & Z]
  }


  private def makeOverrides(using Quotes)(
    effectSymbol: quotes.reflect.Symbol,
    sigSymbol: quotes.reflect.Symbol,
    sigType: quotes.reflect.TypeRepr,
    accumDefDef: ListBuffer[quotes.reflect.DefDef]
  ): List[quotes.reflect.Symbol] =
    import quotes.reflect.*
    given Printer[TypeRepr] = Printer.TypeReprStructure
    given Printer[Tree] = Printer.TreeStructure

    val thisEffectType: TypeRepr = This(effectSymbol).tpe

    def makeLambdaParamType(param: TypeRepr): TypeRepr =
      val bounds = TypeBounds(param, param)
      val refinement = Refinement(TypeRepr.of[Sig], "ThisEffect", bounds)
      AndType(sigType, refinement)

    sigSymbol
      .declaredMethods
      .filter { method => method.flags.is(Flags.Deferred) && !method.flags.is(Flags.Synthetic) }
      .flatMap { abstractMethodSym =>
        debug(s"Abstract method found: ${abstractMethodSym.name}")

        val newMethodType = transformTypeRepr(
          original = abstractMethodSym.info,
          oldType = sigType,
          newType = thisEffectType,
        )

        debug(s"  original type = ${abstractMethodSym.info.show}")
        debug(s"  uppdated type = ${newMethodType.show}")

        extractMethodResultTypes(newMethodType).map { case (resultType, paramAType, paramUType) =>
          val concreteMethodSym = Symbol.newMethod(
            parent = effectSymbol,
            name = abstractMethodSym.name,
            tpe = newMethodType,
            flags = Flags.Override,
            privateWithin = Symbol.noSymbol
          )


          val defDef = DefDef(
            symbol = concreteMethodSym,
            paramss =>
              debug(s"   paramss = $paramss")
              debug(s"   A = ${paramAType.show}")
              debug(s"   U = ${paramUType.show}")

              val lambda = createLambda(
                methodSymbol = concreteMethodSym,
                methodType = newMethodType,
                methodParamss = paramss,
                lambdaParamType = makeLambdaParamType(paramUType),
                resultType = resultType,
              )
              val performUnapplied = Select.unique(This(effectSymbol), "performNoInline")
              val performApplied1 = TypeApply(performUnapplied, List(Inferred(paramAType), Inferred(paramUType)))
              val performApplied2 = Apply(performApplied1, List(lambda))
              Some(performApplied2)
          )

          accumDefDef += defDef
          concreteMethodSym
        }
      }


  private def createLambda(using Quotes)(
    methodSymbol : quotes.reflect.Symbol,
    methodType : quotes.reflect.TypeRepr,
    methodParamss: List[List[quotes.reflect.Tree]],
    lambdaParamType : quotes.reflect.TypeRepr,
    resultType : quotes.reflect.TypeRepr,
  ): quotes.reflect.Term =
    import quotes.reflect.*
    given Printer[TypeRepr] = Printer.TypeReprStructure
    given Printer[Tree] = Printer.TreeStructure

    debug(s"createLambda: ")
    debug(s"  methodType       = ${methodType.show}")
    debug(s"  lambdaParamType  = ${lambdaParamType.show}")
    debug(s"  resultType       = ${resultType.show}")

    Lambda(
      owner = methodSymbol,
      tpe = MethodType(List("z"))(_ => List(lambdaParamType), _ => resultType),
      rhsFn = (_, lambdaParams) =>
        val zParam = lambdaParams.head.asInstanceOf[Term]
        val methodTerm = Select(zParam, methodSymbol)
        applyMethodParamss(
          appliedTerm = methodTerm,
          methodType = methodType,
          paramss = methodParamss,
        )
      )


  private def applyMethodParamss(using Quotes)(
    appliedTerm: quotes.reflect.Term,
    methodType: quotes.reflect.TypeRepr,
    paramss: List[List[quotes.reflect.Tree]],
  ): quotes.reflect.Term =
    import quotes.reflect.*
    given Printer[TypeRepr] = Printer.TypeReprStructure
    given Printer[Tree] = Printer.TreeStructure

    debug(s">>> applyMethodParamss: appliedTerm = ${appliedTerm.show}")
    debug(s">>> applyMethodParamss: paramss     = ${paramss.map(_.map(_.show))}")
    paramss match
      case Nil => appliedTerm
      case params :: moreParamss =>
        methodType match
          case typ: MethodType =>
            val appliedTerm2 = Apply(appliedTerm, params.asInstanceOf[List[Term]])
            applyMethodParamss(appliedTerm2, typ.resType, moreParamss)
          case typ: PolyType =>
            val appliedTerm2 = TypeApply(appliedTerm, params.asInstanceOf[List[TypeTree]])
            applyMethodParamss(appliedTerm2, typ.resType, moreParamss)


  // Tried to use `TypeRepr.substitute`, but it doesn't work for this case.
  // The intention is to replace `ThisType(oldType)` with `ThisType(newType)`.
  // For simplicity sake, `newType` is already pre-wrapped in `ThisType(_)` by the caller.
  def transformTypeRepr(using Quotes)(
    original: quotes.reflect.TypeRepr,
    oldType: quotes.reflect.TypeRepr,
    newType: quotes.reflect.TypeRepr,
  ): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    given Printer[TypeRepr] = Printer.TypeReprStructure
    given Printer[Tree] = Printer.TreeStructure

    // used by ParamRef case
    val cache = new collection.mutable.HashMap[TypeRepr, TypeRepr]
    def caching[A](k: TypeRepr)(a: => A): TypeRepr => A =
      v =>
        if !cache.contains(k)
          then cache(k) = v
        a


    def loopTypeBounds(x: TypeBounds): TypeBounds =
      x match
        case TypeBounds(lo, hi) => TypeBounds(loop(lo), loop(hi))


    def loop(original: TypeRepr): TypeRepr =
      original match
        case TermRef(qualifier, name) => TermRef(loop(qualifier), name)

        // OMG there is no `TypeRef.apply`
        case tr @ TypeRef(qualifier, name) => loop(qualifier).select(tr.typeSymbol)

        case ThisType(underlying) =>
          if underlying =:= oldType then
            newType
          else
            original

        case AndType(left, right) => AndType(loop(left), loop(right))

        case OrType(left, right) => OrType(loop(left), loop(right))

        case ByNameType(underlying) => ByNameType(loop(underlying))

        case AppliedType(tycon, args) => AppliedType(loop(tycon), args.map(loop))

        case MethodType(paramNames, paramTypes, resType) =>
          MethodType(paramNames)(
            caching(original)(paramTypes.map(loop)),
            caching(original)(loop(resType))
          )

        case PolyType(paramNames, paramBounds, resType) =>
          PolyType(paramNames)(
            caching(original)(paramBounds.map(loopTypeBounds)),
            caching(original)(loop(resType))
          )

        case TypeLambda(paramNames, paramBounds, resType) =>
          TypeLambda(
            paramNames,
            caching(original)(paramBounds.map(loopTypeBounds)),
            caching(original)(loop(resType))
          )

        case x: TypeBounds => loopTypeBounds(x)

        case AnnotatedType(underlying, annot) => AnnotatedType(loop(underlying), annot)

        case MatchType(bound, scrutinee, cases) =>
          MatchType(
            loop(bound),
            loop(scrutinee),
            cases.map(loop)
          )

        case ParamRef(t, i) =>
          cache(t) match
            case x: PolyType => x.param(i)
            case x: MethodType => x.param(i)
            case x: TypeLambda => x.param(i)

        case _ => original

    loop(original)
  }


  private def extractMethodResultTypes(using Quotes)(methodType: quotes.reflect.TypeRepr): Option[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] =
    import quotes.reflect.*
    given Printer[TypeRepr] = Printer.TypeReprStructure
    given Printer[Tree] = Printer.TreeStructure

    def loop(typ: TypeRepr): Option[(TypeRepr, TypeRepr, TypeRepr)] =
      typ match
        case x: MethodType => loop(x.resType)
        case x: PolyType => loop(x.resType)
        case ByNameType(x) => loop(x)
        case x @ AppliedType(_, List(a, b)) => Some((x, a, b))
        case _ =>
          debug(s"extractMethodResultTypes: ${typ.show}")
          None

    loop(methodType)
