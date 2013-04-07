package scala

import language.experimental.macros
import language.higherKinds

import reflect.macros.Context

package object idioms extends FunctorInstances with SemiIdiomInstances with IdiomInstances {
  def idiom[F[_]](code: _): _ = macro idiomImpl
  def idiomImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree)), _) = c.macroApplication

    c.macroApplication.updateAttachment(resolveIdiomaticContextByType(c)(typeTree))

    code
  }

  object idiom {
    def apply(idiom: Any)(code: _): _ = macro idiomImpl
    def idiomImpl(c: Context)(idiom: c.Expr[Any])(code: c.Tree): c.Tree = {
      import c.universe._

      val Expr(instance) = idiom

      c.macroApplication.updateAttachment(resolveIdiomaticContextByTerm(c)(instance))

      code
    }
  }

  def $[F[_]](code: _): _ = macro $impl[F]
  def $impl[F[_]](c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree: TypeTree)), _) = c.macroApplication

    val idiomaticContext = if (typeTree.original != null)
                             resolveIdiomaticContextByType(c)(typeTree)
                           else
                             resolveIdiomaticContext(c)

    expandBrackets(c)(code, idiomaticContext).asInstanceOf[Tree]
  }

  private def resolveIdiomaticContextByType(c: Context)(typeTree: c.Tree) = {
    import c.universe._

    val tpe = typeTree.tpe

    val functorTypeRef = TypeRef(NoPrefix, typeOf[Functor[Any]].typeSymbol, List(tpe))
    val functorInstance = c.inferImplicitValue(functorTypeRef)

    if (functorInstance == EmptyTree)
      c.abort(typeTree.pos, s"Unable to find $functorTypeRef instance in implicit scope")

    val map = Select(functorInstance, TermName("map"))

    val pure = {
      val pointedTypeRef = TypeRef(NoPrefix, typeOf[Pointed[Any]].typeSymbol, List(tpe))
      val pointedInstance = c.inferImplicitValue(pointedTypeRef)

      if (pointedInstance == EmptyTree) None
      else Some(Select(pointedInstance, TermName("pure")))
    }

    val app = {
      val semiIdiomTypeRef = TypeRef(NoPrefix, typeOf[SemiIdiom[Any]].typeSymbol, List(tpe))
      val semiIdiomInstance = c.inferImplicitValue(semiIdiomTypeRef)

      if (semiIdiomInstance == EmptyTree) None
      else Some(Select(semiIdiomInstance, TermName("app")))
    }

    IdiomaticContext(tpe, map, pure, app)
  }

  private def resolveIdiomaticContextByTerm(c: Context)(instance: c.Tree) = {
    import c.universe._

    def constructIdiom(instance: Tree, tpe: Type) = {
      val pure = Select(instance, TermName("pure"))
      constructSemiIdiom(instance, tpe).copy(pure = Some(pure))
    }

    def constructSemiIdiom(instance: Tree, tpe: Type) = {
      val app = Select(instance, TermName("app"))
      constructFunctor(instance, tpe).copy(app = Some(app))
    }

    def constructPointed(instance: Tree, tpe: Type) = {
      val pure = Select(instance, TermName("pure"))
      constructFunctor(instance, tpe).copy(pure = Some(pure))
    }

    def constructFunctor(instance: Tree, tpe: Type) = {
      val map = Select(instance, TermName("map"))
      val TypeRef(_, _, List(typeArg)) = tpe
      IdiomaticContext(typeArg, map, None, None)
    }

    def constructIdiomaticContext(tpe: Type): Option[IdiomaticContext] = tpe match {
      case RefinedType(parents, _) ⇒
        parents.view.flatMap(constructIdiomaticContext _).headOption
      case TypeRef(_, sym, _) ⇒
        PartialFunction.condOpt(sym.fullName) {
          case "scala.idioms.Functor"  ⇒ constructFunctor(instance, tpe)
          case "scala.idioms.FunctorT" ⇒ constructFunctor(instance, tpe.baseType(tpe.baseClasses(1)))
          case "scala.idioms.Pointed"  ⇒ constructPointed(instance, tpe)
          case "scala.idioms.PointedT" ⇒ constructPointed(instance, tpe.baseType(tpe.baseClasses(1)))
          case "scala.idioms.SemiIdiom"  ⇒ constructSemiIdiom(instance, tpe)
          case "scala.idioms.SemiIdiomT" ⇒ constructSemiIdiom(instance, tpe.baseType(tpe.baseClasses(1)))
          case "scala.idioms.Idiom"  ⇒ constructIdiom(instance, tpe)
          case "scala.idioms.IdiomT" ⇒ constructIdiom(instance, tpe.baseType(tpe.baseClasses(1)))
        }
      case _ ⇒ None
    }

    constructIdiomaticContext(instance.tpe) getOrElse {
      c.abort(instance.pos, "Not an idiom")
    }
  }

  private def resolveIdiomaticContext(c: Context) = {
    val idiomaticContext = for {
      context ← c.openMacros.view
      attachments = context.macroApplication.attachments
      idiomaticContext ← attachments.get[IdiomaticContext]
    } yield idiomaticContext

    idiomaticContext.headOption getOrElse {
      c.abort(c.enclosingPosition, "Idiom brackets outside of idiom block")
    }
  }

  private def expandBrackets(c: Context)(code: c.Tree, idiomaticContext: IdiomaticContext): c.Tree = {
    import c.universe._

    val IdiomaticContext(idiom: Type, map: Tree, optPure: Option[Tree], optApp: Option[Tree]) = idiomaticContext

    def expand(expr: Tree) = {
      def produceMap(lambda: Tree, map: Tree, arg: Tree) = Apply(Apply(map, List(lambda)), List(arg))

      def producePure(lambda: Tree, pure: Tree) = Apply(pure, List(lambda))

      def produceApp(lambda: Tree, app: Tree, args: List[Tree]) =
        args.foldLeft(lambda) {
          (tree, arg) ⇒ Apply(Apply(app, List(tree)), List(arg))
        }

      def produceApplication(lambda: Tree): List[Tree] ⇒ Tree = {
        case Nil ⇒ optPure match {
          case Some(pure) ⇒ producePure(lambda, pure)
          case None ⇒ c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement Pointed")
        }
        case arg :: Nil ⇒ produceMap(lambda, map, arg)
        case arg :: restArgs ⇒ optApp match {
          case Some(app) ⇒ produceApp(produceMap(lambda, map, arg), app, restArgs)
          case None ⇒ c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement SemiIdiom")
        }
      }

      val (lambda, args) = composeLambda(expr)

      produceApplication(lambda)(args)
    }

    def resolveLiftedType(tpe: Type): Type = {
      val TypeRef(_, _, typeArgs) = tpe
      idiom match {
        case PolyType(List(wildcard), typeRef: TypeRef) ⇒
          // When idiom is passed as type lambda, we need to take the type
          // from wildcard position, so we zip through both typerefs to seek for a substitution
          def findSubstitution(wildcardedType: Type, concreteType: Type): Option[Type] = {
            if (wildcardedType.typeSymbol == wildcard)
              Some(concreteType)
            else
              (wildcardedType, concreteType) match {
                case (wctpe: TypeRef, ctpe: TypeRef) ⇒
                  wctpe.args zip ctpe.args map {
                    case (wct, at) ⇒ findSubstitution(wct, at)
                  } collectFirst {
                    case Some(t) ⇒ t
                  }
                case _ ⇒ None
              }
          }
          findSubstitution(typeRef, tpe) getOrElse {
            c.abort(c.enclosingPosition, "Unable to lift argument to an idiom")
          }
        case _ ⇒
          // This only works for type constructor of one argument
          // TODO: provide implementation for n-arity type constructors
          typeArgs.head
      }
    }

    def composeLambda(code: Tree) = {
      val (body, binds) = extractLambdaBody(code)
      val lambda = binds.foldRight(body) {
        (bind, tree) ⇒
          val (name, arg) = bind
          val skolem = resolveLiftedType(c.typeCheck(arg).tpe)
          val valdef = ValDef(Modifiers(), name, TypeTree(skolem), EmptyTree)
          Function(List(valdef), tree)
      }
      val (_, args) = binds.unzip
      (lambda, args)
    }

    def isLifted(arg: Tree) = {
      val baseClasses = c.typeCheck(arg.duplicate, silent=true).tpe.baseClasses
      baseClasses contains idiom.typeSymbol
    }

    type Binds = List[(TermName, Tree)]

    def extractLambdaBody(code: Tree): (Tree, Binds) = code match {
      case expr if isLifted(expr) ⇒
        val name = TermName(c.freshName("arg$"))
        (Ident(name), List(name → expr))

      case Apply(expr, args) ⇒
        val (body, binds) = extractLambdaBody(expr)
        val (newargs, newbinds) = args.map(extractLambdaBody(_)).unzip
        (Apply(body, newargs), binds ++ newbinds.flatten)

      case Select(arg, method) ⇒
        val (newarg, binds) = extractLambdaBody(arg)
        (Select(newarg, method), binds)

      case expr ⇒
        (expr, Nil)
    }

    expand(code)
  }
}

package idioms {
  private[idioms] case class IdiomaticContext(tpe: Any, map: Any, pure: Option[Any], app: Option[Any])
}