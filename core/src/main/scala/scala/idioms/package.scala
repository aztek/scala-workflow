package scala

import language.experimental.macros
import language.higherKinds

import reflect.macros.Context

package object idioms extends FunctorInstances with SemiIdiomInstances with IdiomInstances {
  def idiom[F[_]](code: _): _ = macro idiomImpl
  def idiomImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree)), _) = c.macroApplication

    c.macroApplication.updateAttachment(contextFromType(c)(typeTree))

    code
  }

  object idiom {
    def apply(idiom: Any)(code: _): _ = macro idiomImpl
    def idiomImpl(c: Context)(idiom: c.Expr[Any])(code: c.Tree): c.Tree = {
      import c.universe._

      val Expr(instance) = idiom

      c.macroApplication.updateAttachment(contextFromTerm(c)(instance))

      code
    }
  }

  def $[F[_]](code: _): _ = macro $impl
  def $impl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree: TypeTree)), _) = c.macroApplication

    val idiomaticContext = if (typeTree.original != null)
                             contextFromType(c)(typeTree)
                           else
                             contextFromEnclosingIdiom(c)

    expandBrackets(c)(code, idiomaticContext).asInstanceOf[Tree]
  }

  private def contextFromType(c: Context)(typeTree: c.Tree) = {
    import c.universe._

    val tpe = typeTree.tpe

    val workflowTypeRef = TypeRef(NoPrefix, typeOf[Workflow[Any]].typeSymbol, List(tpe))
    val workflowInstance = c.inferImplicitValue(workflowTypeRef)

    if (workflowInstance == EmptyTree)
      c.abort(typeTree.pos, s"Unable to find $workflowTypeRef instance in implicit scope")

    val map = {
      val mappingTypeRef = TypeRef(NoPrefix, typeOf[Mapping[Any]].typeSymbol, List(tpe))
      val mappingInstance = c.inferImplicitValue(mappingTypeRef)

      if (mappingInstance == EmptyTree) None
      else Some(q"$mappingInstance.map")
    }

    val point = {
      val pointingTypeRef = TypeRef(NoPrefix, typeOf[Pointing[Any]].typeSymbol, List(tpe))
      val pointingInstance = c.inferImplicitValue(pointingTypeRef)

      if (pointingInstance == EmptyTree) None
      else Some(q"$pointingInstance.point")
    }

    val app = {
      val applyingTypeRef = TypeRef(NoPrefix, typeOf[Applying[Any]].typeSymbol, List(tpe))
      val applyingInstance = c.inferImplicitValue(applyingTypeRef)

      if (applyingInstance == EmptyTree) None
      else Some(q"$applyingInstance.app")
    }

    IdiomaticContext(tpe, map, point, app)
  }

  private def implementsMapping(c: Context)(tpe: c.Type)  = tpe.baseClasses exists (_.fullName == "scala.idioms.Mapping")
  private def implementsPointing(c: Context)(tpe: c.Type) = tpe.baseClasses exists (_.fullName == "scala.idioms.Pointing")
  private def implementsApplying(c: Context)(tpe: c.Type) = tpe.baseClasses exists (_.fullName == "scala.idioms.Applying")

  private def contextFromTerm(c: Context)(instance: c.Tree): IdiomaticContext = {
    import c.universe._

    val tpe = instance.tpe

    val workflowSymbol = tpe.baseClasses find (_.fullName == "scala.idioms.Workflow") getOrElse {
      c.abort(c.enclosingPosition, "Not a workflow instance")
    }

    val TypeRef(_, _, List(workflowType)) = tpe.baseType(workflowSymbol)

    val map = if (implementsMapping(c)(tpe)) Some(q"$instance.map") else None
    val point = if (implementsPointing(c)(tpe)) Some(q"$instance.point") else None
    val app = if (implementsApplying(c)(tpe)) Some(q"$instance.app") else None

    IdiomaticContext(workflowType, map, point, app)
  }

  private def contextFromEnclosingIdiom(c: Context) = {
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

    val IdiomaticContext(idiom: Type, optMap: Option[Tree], optPoint: Option[Tree], optApp: Option[Tree]) = idiomaticContext

    def expand(expr: Tree) = {
      def produceApplication(lambda: Tree): List[Tree] ⇒ Tree = {
        case Nil ⇒
          optPoint match {
            case Some(point) ⇒ q"$point($lambda)"
            case None ⇒ c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement Pointing")
          }
        case arg :: Nil ⇒
          optMap match {
            case Some(map) ⇒ q"$map($lambda)($arg)"
            case None ⇒ c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement Mapping")
          }
        case arg :: args ⇒
          optApp match {
            case Some(app) ⇒
              optMap match {
                case Some(map) ⇒
                  args.foldLeft(q"$map($lambda)($arg)") {
                    (tree, arg) ⇒ q"$app($tree)($arg)"
                  }
                case None ⇒ c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement Mapping")
              }
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
          val tpe = TypeTree(resolveLiftedType(c.typeCheck(arg).tpe))
          q"($name: $tpe) ⇒ $tree"
      }
      val (_, args) = binds.unzip
      (lambda, args)
    }

    def isLifted(arg: Tree) = {
      val baseClasses = c.typeCheck(arg.duplicate, silent=true).tpe.baseClasses
      baseClasses contains idiom.typeSymbol
    }

    type Binds = List[(TermName, Tree)]

    def extractLambdaBody: Tree ⇒ (Tree, Binds) = {
      case expr if isLifted(expr) ⇒
        val name = TermName(c.freshName("arg$"))
        (q"$name", List(name → expr))

      case Apply(fun, args) ⇒ // cant's use quasiquotes here, SI-7400
        val (newfun, binds) = extractLambdaBody(fun)
        val (newargs, newbinds) = args.map(extractLambdaBody).unzip
        (q"$newfun(..$newargs)", binds ++ newbinds.flatten)

      case q"$value.$method" ⇒
        val (newvalue, binds) = extractLambdaBody(value)
        (q"$newvalue.$method", binds)

      case expr ⇒
        (expr, Nil)
    }

    expand(code)
  }
}

package idioms {
  private[idioms] case class IdiomaticContext(tpe: Any, map: Option[Any], point: Option[Any], app: Option[Any])
}