package scala

import language.experimental.macros
import language.higherKinds

import scala.reflect.macros.{TypecheckException, Context}

package object workflow extends FunctorInstances with SemiIdiomInstances with MonadInstances {
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

    val workflowContext = if (typeTree.original != null)
                            contextFromType(c)(typeTree)
                          else
                            contextFromEnclosingIdiom(c)

    expandBrackets(c)(code, workflowContext).asInstanceOf[Tree]
  }

  private def contextFromType(c: Context)(typeTree: c.Tree) = {
    import c.universe._

    val tpe = typeTree.tpe

    val typeRef = TypeRef(NoPrefix, typeOf[Workflow[Any]].typeSymbol, List(tpe))
    val instance = c.inferImplicitValue(typeRef)

    if (instance == EmptyTree)
      c.abort(typeTree.pos, s"Unable to find $typeRef instance in implicit scope")

    WorkflowContext(tpe, instance)
  }

  private def contextFromTerm(c: Context)(instance: c.Tree): WorkflowContext = {
    import c.universe._

    val workflowSymbol = instance.tpe.baseClasses find (_.fullName == "scala.workflow.Workflow") getOrElse {
      c.abort(c.enclosingPosition, "Not a workflow instance")
    }

    val TypeRef(_, _, List(tpe)) = instance.tpe.baseType(workflowSymbol)

    WorkflowContext(tpe, instance)
  }

  private def contextFromEnclosingIdiom(c: Context) = {
    val workflowContext = for {
      context ← c.openMacros.view
      attachments = context.macroApplication.attachments
      workflowContext ← attachments.get[WorkflowContext]
    } yield workflowContext

    workflowContext.headOption getOrElse {
      c.abort(c.enclosingPosition, "Idiom brackets outside of idiom block")
    }
  }

  private def expandBrackets(c: Context)(code: c.Tree, workflowContext: WorkflowContext): c.Tree = {
    import c.universe._

    val WorkflowContext(idiom: Type, instance: Tree) = workflowContext

    val implementsMapping  = instance.tpe.baseClasses exists (_.fullName == "scala.workflow.Mapping")
    val implementsPointing = instance.tpe.baseClasses exists (_.fullName == "scala.workflow.Pointing")
    val implementsApplying = instance.tpe.baseClasses exists (_.fullName == "scala.workflow.Applying")

    def expand(expr: Tree) = {
      def produceApplication(lambda: Tree): List[Tree] ⇒ Tree = {
        case Nil ⇒
          if (!implementsPointing)
            c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement Pointing")

          q"$instance.point($lambda)"

        case arg :: Nil ⇒
          if (!implementsMapping)
            c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement Mapping")

          q"$instance.map($lambda)($arg)"

        case arg :: args ⇒
          if (!implementsApplying)
            c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement Applying")

          if (!implementsMapping)
            c.abort(c.enclosingPosition, s"Enclosing idiom for type $idiom does not implement Mapping")

          args.foldLeft(q"$instance.map($lambda)($arg)") {
            (tree, arg) ⇒ q"$instance.app($tree)($arg)"
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

    def typeCheck(tree: Tree, binds: Binds): Option[Tree] = {
      val vals = binds map { case (name, tpe, _) ⇒ q"val $name : ${TypeTree(tpe).duplicate} = ???" }
      try {
        Some(c.typeCheck(q"{ ..$vals; ${tree.duplicate} }"))
      } catch {
        case e: TypecheckException if e.msg contains "follow this method with `_'" ⇒
          try {
            val newvals = binds map { case (name, tpe, _) ⇒ q"val $name : ${TypeTree(tpe).duplicate} = ???" }
            Some(c.typeCheck(q"{ ..$newvals; ${tree.duplicate} _ }"))
          } catch {
            case e: TypecheckException if e.msg contains "ambiguous reference" ⇒ Some(EmptyTree)
            case e: Exception ⇒ None
          }
        case e: TypecheckException if e.msg contains "missing arguments for constructor" ⇒
          try {
            val newvals = binds map { case (name, tpe, _) ⇒ q"val $name : ${TypeTree(tpe).duplicate} = ???" }
            Some(c.typeCheck(q"{ ..$newvals; (${tree.duplicate})(_) }"))
          } catch {
            case e: TypecheckException if !(e.msg contains "too many arguments for constructor") ⇒ Some(EmptyTree)
            case e: Exception ⇒ None
          }
        case e: TypecheckException if e.msg contains "ambiguous reference" ⇒ Some(EmptyTree)
        case e: Exception ⇒ None
      }
    }

    def isLifted(tpe: Type) = tpe.baseClasses contains idiom.typeSymbol

    type Bind  = (TermName, Type, Tree)
    type Binds = List[Bind]

    def composeLambda(code: Tree) = {
      val (body, binds) = rewrite(List.empty[Bind])(code)
      val lambda = binds.foldLeft(body) {
        (tree, bind) ⇒
          val (name, tpe, _) = bind
          q"($name: ${TypeTree(tpe)}) ⇒ $tree"
      }
      val (_, _, args) = binds.reverse.unzip3
      (lambda, args)
    }

    def rewrite(binds: Binds): Tree ⇒ (Tree, Binds) = {
      case Apply(fun, args) ⇒
        val (newfun,  funbinds)  = extractLambdaBody(binds)(fun)
        val (newargs, argsbinds) = args.map(extractLambdaBody(funbinds)).unzip
        extractLambdaBody(argsbinds.flatten.distinct)(q"$newfun(..$newargs)")

      case Select(value, method) ⇒
        val (newvalue, newbinds) = extractLambdaBody(binds)(value)
        extractLambdaBody(newbinds)(q"$newvalue.$method")

      case value @ Literal(_) ⇒
        extractLambdaBody(binds)(value)

      case ident @ Ident(_) ⇒
        extractLambdaBody(binds)(ident)

      case expr ⇒
        c.abort(expr.pos, "Unsupported expression " + showRaw(expr))
    }

    def extractLambdaBody(binds: Binds): Tree ⇒ (Tree, Binds) = expr ⇒ {
      typeCheck(expr, binds) match {
        case None ⇒
          rewrite(binds)(expr)

        case Some(typeTree) if isLifted(typeTree.tpe) ⇒
          val name = TermName(c.freshName("arg$"))
          val tpe  = resolveLiftedType(typeTree.tpe)
          val bind = (name, tpe, expr)
          (q"$name", bind :: binds)

        case _ ⇒
          (expr, binds)
      }
    }

    expand(code)
  }
}

package workflow {
  private[workflow] case class WorkflowContext(tpe: Any, instance: Any)
}