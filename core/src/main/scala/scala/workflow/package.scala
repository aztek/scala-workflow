package scala

import language.experimental.macros
import language.higherKinds

import scala.reflect.macros.{TypecheckException, Context}

package object workflow extends FunctorInstances with SemiIdiomInstances with MonadInstances {
  def context[F[_]](code: _): _ = macro contextImpl
  def contextImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree)), _) = c.macroApplication

    c.macroApplication.updateAttachment(contextFromType(c)(typeTree))

    code
  }

  object context {
    def apply(workflow: Any)(code: _): _ = macro contextImpl
    def contextImpl(c: Context)(workflow: c.Expr[Any])(code: c.Tree): c.Tree = {
      import c.universe._

      val Expr(instance) = workflow

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
      c.abort(instance.pos, "Not a workflow instance")
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
      c.abort(c.enclosingPosition, "Workflow brackets outside of `context' block")
    }
  }

  private def expandBrackets(c: Context)(code: c.Tree, workflowContext: WorkflowContext): c.Tree = {
    import c.universe._

    val WorkflowContext(workflow: Type, instance: Tree) = workflowContext

    def resolveLiftedType(tpe: Type): Option[Type] =
      tpe.baseType(workflow.typeSymbol) match {
        case baseType @ TypeRef(_, _, typeArgs) ⇒
          workflow match {
            case PolyType(List(wildcard), typeRef: TypeRef) ⇒
              // When workflow is passed as type lambda, we need to take the type
              // from wildcard position, so we zip through both typerefs to seek for a substitution
              def findSubstitution(wildcardedType: Type, concreteType: Type): Option[Type] = {
                if (wildcardedType.typeSymbol == wildcard)
                  Some(concreteType)
                else
                  (wildcardedType, concreteType) match {
                    case (wctpe: TypeRef, ctpe: TypeRef) ⇒
                      wctpe.args zip ctpe.args find {
                        case (wct, at) ⇒ !(wct =:= at)
                      } flatMap {
                        case (wct, at) ⇒ findSubstitution(wct, at)
                      }
                    case _ ⇒ None
                  }
              }
              findSubstitution(typeRef, baseType)
            case _ ⇒
              // This only works for type constructor of one argument
              // TODO: provide implementation for n-arity type constructors
              typeArgs.headOption
          }
        case _ ⇒ None
      }

    type Rebind  = (TermName, TypeTree, Tree)
    type Rebinds = List[Rebind]

    def typeCheck(tree: Tree, rebinds: Rebinds): Option[Tree] = {
      val vals = rebinds map { case (name, tpe, _) ⇒ q"val $name: $tpe = ???" }
      try {
        Some(c.typeCheck(q"{ ..$vals; ${tree.duplicate} }"))
      } catch {
        case e: TypecheckException if e.msg contains "follow this method with `_'" ⇒ Some(EmptyTree)
        case e: TypecheckException if e.msg contains "missing arguments for constructor" ⇒
          try {
            val newvals = rebinds map { case (name, tpe, _) ⇒ q"val $name: $tpe = ???" }
            Some(c.typeCheck(q"{ ..$newvals; (${tree.duplicate})(_) }"))
          } catch {
            case e: TypecheckException if !(e.msg contains "too many arguments for constructor") ⇒ Some(EmptyTree)
            case e: Exception ⇒ None
          }
        case e: TypecheckException if e.msg contains "ambiguous reference" ⇒ Some(EmptyTree)
        case e: TypecheckException if (e.msg contains "package") && (e.msg contains "is not a value") ⇒ Some(EmptyTree)
        case e: Exception ⇒ None
      }
    }

    def rewrite(rebinds: Rebinds): Tree ⇒ (Rebinds, Tree) = {
      case Apply(fun, args) ⇒
        val (funrebinds,  newfun)  = rewrite(rebinds)(fun)
        val (argsrebinds, newargs) = args.map(rewrite(funrebinds)).unzip
        extractBinds(argsrebinds.reverse.flatten.distinct, q"$newfun(..$newargs)")

      case Select(value, method) ⇒
        val (newrebinds, newvalue) = rewrite(rebinds)(value)
        extractBinds(newrebinds, q"$newvalue.$method")

      case value @ Literal(_) ⇒ extractBinds(rebinds, value)

      case ident @ Ident(_) ⇒ extractBinds(rebinds, ident)

      case constructor @ New(_) ⇒ extractBinds(rebinds, constructor)

      case expr ⇒
        c.abort(expr.pos, "Unsupported expression " + showRaw(expr))
    }

    def extractBinds(rebinds: Rebinds, expr: Tree) =
      typeCheck(expr, rebinds) match {
        case Some(typeTree) ⇒
          resolveLiftedType(typeTree.tpe) match {
            case Some(tpe) ⇒
              val name = TermName(c.freshName("arg$"))
              val rebind = (name, TypeTree(tpe), expr)
              (rebinds :+ rebind, q"$name")

            case None ⇒ (rebinds, expr)
          }
        case None ⇒ rewrite(rebinds)(expr)
      }

    val implementsMapping  = instance.tpe.baseClasses exists (_.fullName == "scala.workflow.Mapping")
    val implementsPointing = instance.tpe.baseClasses exists (_.fullName == "scala.workflow.Pointing")
    val implementsApplying = instance.tpe.baseClasses exists (_.fullName == "scala.workflow.Applying")
    val implementsBinding  = instance.tpe.baseClasses exists (_.fullName == "scala.workflow.Binding")

    def applyRebinds(rebinds: Rebinds): Tree ⇒ Tree = {
      def lambda: Rebind ⇒ Tree ⇒ Tree = {
        case (name, tpe, _) ⇒
          expr ⇒ q"($name: $tpe) ⇒ $expr"
      }

      def lambdas: Rebinds ⇒ Tree ⇒ Tree = _ map lambda reduce (_ compose _)

      def point: Tree ⇒ Tree = {
        if (!implementsPointing)
          c.abort(c.enclosingPosition, s"Enclosing workflow for type $workflow does not implement Pointing")
        expr ⇒ q"$instance.point($expr)"
      }

      def map: Rebind ⇒ Tree ⇒ Tree = {
        case (_, _, value) ⇒
          if (!implementsMapping)
            c.abort(c.enclosingPosition, s"Enclosing workflow for type $workflow does not implement Mapping")
          expr ⇒ q"$instance.map($expr)($value)"
      }

      def app: Rebind ⇒ Tree ⇒ Tree = {
        case (_, _, value) ⇒
          if (!implementsApplying)
            c.abort(c.enclosingPosition, s"Enclosing workflow for type $workflow does not implement Applying")
          expr ⇒ q"$instance.app($expr)($value)"
      }

      def apps: Rebinds ⇒ Tree ⇒ Tree = _ map app reduce (_ compose _)

      def bind: Rebind ⇒ Tree ⇒ Tree = {
        case (_, _, value) ⇒
          if (!implementsBinding)
            c.abort(c.enclosingPosition, s"Enclosing workflow for type $workflow does not implement Binding")
          expr ⇒ q"$instance.bind($expr)($value)"
      }

      def binds: Rebinds ⇒ Tree ⇒ Tree = _ map bind reduce (_ compose _)

      rebinds match {
        case Nil ⇒ point
        case rebind :: Nil ⇒ map(rebind) compose lambda(rebind)
        case rebind :: rebinds ⇒ apps(rebinds.reverse) compose map(rebind) compose lambdas(rebind :: rebinds)
      }
    }

    val (rebinds, expr) = rewrite(List.empty[Rebind])(code)
    applyRebinds(rebinds)(expr)
  }
}

package workflow {
  private[workflow] case class WorkflowContext(tpe: Any, instance: Any)
}