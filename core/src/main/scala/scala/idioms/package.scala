package scala

import language.experimental.macros
import language.higherKinds
import reflect.macros.{TypecheckException, Context}

package object idioms {
  def idiom[F[_]](code: _): _ = macro idiomImpl
  def idiomImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree)), _) = c.macroApplication

    c.macroApplication.updateAttachment(resolveIdiomaticContext(c)(typeTree))

    code
  }

  object idiom {
    def apply[F[_]](idiom: Idiom[F])(code: _): _ = macro idiomImpl[F]
    def idiomImpl[F[_]](c: Context)(idiom: c.Expr[Idiom[F]])(code: c.Tree): c.Tree = {
      import c.universe._

      val Expr(instance) = idiom

      val pure = Select(instance, TermName("pure"))
      val app  = Select(instance, TermName("app"))

      val RefinedType(_ :+ TypeRef(_, _, tpe :: _), _) = c.typeCheck(instance.duplicate).tpe

      c.macroApplication.updateAttachment(IdiomaticContext(tpe, pure, app))

      code
    }
  }

  def $[F[_]](code: _): _ = macro $impl[F]
  def $impl[F[_]](c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(typeTree: TypeTree)), _) = c.macroApplication

    val idiomaticContext = if (typeTree.original != null) {
      resolveIdiomaticContext(c)(typeTree)
    } else {
      // No type was provided, look into enclosing idiom block
      def enclosingIdiomaticContext: IdiomaticContext = {
        for (context ← c.openMacros) {
          val attachments = context.macroApplication.attachments
          for (idiomaticContext ← attachments.get[IdiomaticContext]) {
            return idiomaticContext
          }
        }
        c.abort(c.enclosingPosition, "Idiom brackets outside of idiom block")
      }
      enclosingIdiomaticContext
    }
    expandBrackets(c)(code, idiomaticContext).asInstanceOf[Tree]
  }

  private def resolveIdiomaticContext(c: Context)(typeTree: c.Tree) = {
    import c.universe._

    val tpe = typeTree.tpe
    try {
      val typeref = TypeRef(NoPrefix, typeOf[Idiom[Any]].typeSymbol, List(tpe))
      val instance = c.inferImplicitValue(typeref, silent=false)

      val pure = Select(instance, TermName("pure"))
      val app  = Select(instance, TermName("app"))

      IdiomaticContext(tpe, pure, app)
    } catch {
      case e: TypecheckException ⇒
        c.abort(typeTree.pos, s"Unable to find $tpe instance in implicit scope")
    }
  }

  private def expandBrackets(c: Context)(code: c.Tree, idiomaticContext: IdiomaticContext): c.Tree = {
    import c.universe._

    val IdiomaticContext(idiom: Type, pure: Tree, app: Tree) = idiomaticContext

    def expand(expr: Tree) = {
      def liftLambda(lambda: Tree) = Apply(pure, List(lambda))

      def liftApplication(liftedLambda: Tree, args: List[Tree]) =
        args.foldLeft(liftedLambda) {
          (tree, arg) ⇒ Apply(Apply(app, List(tree)), List(arg))
        }

      val (lambda, args) = composeLambda(expr)

      liftApplication(liftLambda(lambda), args)
    }

    def composeLambda(code: Tree) = {
      val (body, binds) = extractLambdaBody(code)
      val lambda = binds.foldRight(body) {
        (bind, tree) ⇒
          val (name, arg) = bind
          val skolem = c.typeCheck(arg).tpe match {
            case TypeRef(_, _, s :: _) ⇒ TypeTree(s)
            case _ ⇒ c.abort(arg.pos, s"Unable to determine lifted type of $arg")
          }
          val valdef = ValDef(Modifiers(), name, skolem, EmptyTree)
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
  private[idioms] case class IdiomaticContext(tpe: Any, pure: Any, app: Any)
}