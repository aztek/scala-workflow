package scala

import language.experimental.macros
import language.higherKinds
import reflect.macros.{TypecheckException, Context}

package object idioms {
  def idiom[F[_]](code: _): _ = macro idiomImpl
  def idiomImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(applicative)), _) = c.macroApplication

    val (pure, app) = resolveApplicative(c)(applicative)

    c.macroApplication.updateAttachment(ApplicativeContext(applicative, pure, app))

    code
  }

  object idiom {
    def apply[F[_]](applicative: Applicative[F])(code: _): _ = macro idiomImpl[F]
    def idiomImpl[F[_]](c: Context)(applicative: c.Expr[Applicative[F]])(code: c.Tree): c.Tree = {
      import c.universe._

      val Expr(instance) = applicative
      val RefinedType(applicativeParents, _) = c.typeCheck(instance.duplicate).tpe
      val TypeRef(_, _, tpe :: _) = applicativeParents.last

      val pure = Select(instance, TermName("pure"))
      val app  = Select(instance, TermName("app"))

      c.macroApplication.updateAttachment(ApplicativeContext(TypeTree(tpe), pure, app))

      code
    }
  }

  def $ (code: _): _ = macro $impl
  def $impl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    def applicativeContext: (TypeTree, Tree, Tree) = {
      for (context ← c.openMacros) {
        val attachments = context.macroApplication.attachments
        for (ApplicativeContext(applicative, pure, app) ← attachments.get[ApplicativeContext]) {
          return (applicative.asInstanceOf[TypeTree], pure.asInstanceOf[Tree], app.asInstanceOf[Tree])
        }
      }
      c.abort(c.enclosingPosition, "Idiom brackets outside of idiom block")
    }

    val (applicative, pure, app) = applicativeContext

    def expandBrackets(expr: Tree) = {
      def liftLambda(lambda: Tree) = Apply(pure, List(lambda))

      def liftApplication(liftedLambda: Tree, args: List[Tree]) =
        args.foldLeft(liftedLambda) {
          (tree, arg) ⇒ Apply(Apply(app, List(tree)), List(arg))
        }

      val (lambda, args) = composeLiftableLambda(expr)

      liftApplication(liftLambda(lambda), args)
    }

    def composeLiftableLambda(code: Tree) = {
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
      baseClasses contains applicative.tpe.typeSymbol
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

    expandBrackets(code)
  }

  private def resolveApplicative(c: Context)(typeTree: c.Tree) = {
    import c.universe._

    val typeref = TypeRef(NoPrefix, typeOf[Applicative[Option]].typeSymbol, List(typeTree.tpe))

    try {
      val instance = c.inferImplicitValue(typeref, silent=false)

      val pure = Select(instance, TermName("pure"))
      val app  = Select(instance, TermName("app"))

      (pure, app)
    } catch {
      case e: TypecheckException ⇒
        c.abort(typeTree.pos, s"Unable to find $typeref instance in implicit scope")
    }
  }
}

package idioms {
  private[idioms] case class ApplicativeContext(tpe: Any, pure: Any, app: Any)
}