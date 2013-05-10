package scala

import language.experimental.macros
import language.higherKinds

import scala.reflect.macros.{TypecheckException, Context}

package object workflow extends FunctorInstances with SemiIdiomInstances with IdiomInstances with MonadInstances {
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

    case class Rebind(name: TermName, tpt: TypeTree, value: Tree, isLocal: Boolean) {
      def isUsedIn(rebinds: Rebinds) = rebinds exists (_.value exists (_ equalsStructure q"$name"))
    }
    type Rebinds = List[Rebind]

    def typeCheck(tree: Tree, rebinds: Rebinds): Option[Tree] = {
      val vals = rebinds map (rebind ⇒ q"val ${rebind.name}: ${rebind.tpt} = ???")
      try {
        Some(c.typeCheck(q"{ ..$vals; ${tree.duplicate} }"))
      } catch {
        case e: TypecheckException if e.msg contains "follow this method with `_'" ⇒ Some(EmptyTree)
        case e: TypecheckException if e.msg contains "missing arguments for constructor" ⇒
          try {
            val newvals = rebinds map (rebind ⇒ q"val ${rebind.name}: ${rebind.tpt} = ???")
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
        extractRebinds(argsrebinds.flatten.distinct, q"$newfun(..$newargs)")

      case Select(value, method) ⇒
        val (newrebinds, newvalue) = rewrite(rebinds)(value)
        extractRebinds(newrebinds, q"$newvalue.$method")

      case ValDef(NoMods, name, tpt, expr) ⇒
        val (newrebinds, newexpr) = rewrite(rebinds)(expr)
        val tpe = typeCheck(newexpr, newrebinds).get.tpe
        val newrebind = Rebind(name, TypeTree(tpe), newexpr, isLocal=true)
        (newrebinds :+ newrebind, q"val $name: $tpt = $newexpr")

      case Block(stat :: stats, expr) ⇒
        val (statrebinds, newstat) = rewrite(rebinds)(stat)
        val (newrebinds, newblock) = rewrite(statrebinds)(q"{ ..$stats; $expr }")
        (newrebinds filterNot (_.isLocal), q"{ $newstat; $newblock }")

      case Block(Nil, expr) ⇒ rewrite(rebinds)(expr)

      case expr @ (_ : Literal | _ : Ident | _ : New) ⇒ extractRebinds(rebinds, expr)

      case expr ⇒
        c.abort(expr.pos, "Unsupported expression " + showRaw(expr))
    }

    def extractRebinds(rebinds: Rebinds, expr: Tree) =
      typeCheck(expr, rebinds) match {
        case Some(tpt) ⇒
          resolveLiftedType(tpt.tpe) match {
            case Some(tpe) ⇒
              val name = TermName(c.freshName("arg$"))
              val rebind = Rebind(name, TypeTree(tpe), expr, isLocal=false)
              (rebinds :+ rebind, q"$name")

            case None ⇒ (rebinds, expr)
          }
        case None ⇒ rewrite(rebinds)(expr)
      }

    def lambda(rebind: Rebind): Tree ⇒ Tree = {
      expr ⇒ q"(${rebind.name}: ${rebind.tpt}) ⇒ $expr"
    }

    val interfaces = instance.tpe.baseClasses map (_.fullName)
    def assertImplements(interface: String) {
      if (!interfaces.contains(interface))
        c.abort(c.enclosingPosition, s"Enclosing workflow for type $workflow does not implement $interface")
    }

    def point: Tree ⇒ Tree = {
      assertImplements("scala.workflow.Pointing")
      expr ⇒ q"$instance.point($expr)"
    }

    def map(rebind: Rebind): Tree ⇒ Tree = {
      assertImplements("scala.workflow.Mapping")
      expr ⇒ q"$instance.map($expr)(${rebind.value})"
    }

    def app(rebind: Rebind): Tree ⇒ Tree = {
      assertImplements("scala.workflow.Applying")
      expr ⇒ q"$instance.app($expr)(${rebind.value})"
    }

    def bind(rebind: Rebind): Tree ⇒ Tree = {
      assertImplements("scala.workflow.Binding")
      expr ⇒ q"$instance.bind($expr)(${rebind.value})"
    }

    def apply: Rebinds ⇒ Tree ⇒ Tree = {
      case Nil ⇒ point
      case rebind :: Nil ⇒ map(rebind) compose lambda(rebind)
      case rebind :: rebinds ⇒
        if (rebind isUsedIn rebinds)
          bind(rebind) compose lambda(rebind) compose apply(rebinds)
        else
          app(rebind) compose apply(rebinds) compose lambda(rebind)
    }

    val (rebinds, expr) = rewrite(List.empty[Rebind])(code)
    apply(rebinds)(expr)
  }
}

package workflow {
  private[workflow] case class WorkflowContext(tpe: Any, instance: Any)
}