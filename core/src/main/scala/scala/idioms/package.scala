package scala

import language.experimental.macros
import language.higherKinds
import reflect.macros.{TypecheckException, Context}

package object idioms {
  def idiom[F[_]](code: _): _ = macro idiomImpl
  def idiomImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val Apply(TypeApply(_, List(applicative)), _) = c.macroApplication

    val (pure, app) = resolveApplicative(c)(applicative.tpe)

    c.macroApplication.updateAttachment(pure)
    c.macroApplication.updateAttachment(app)

    code
  }

  def $ (code: _): _ = macro bracketsImpl
  def bracketsImpl(c: Context)(code: c.Tree): c.Tree = {
    def applicativeContext: (c.universe.TypeTree, c.universe.Tree, c.universe.Tree) = {
      for (context ← c.openMacros) {
        import context.universe._
        context.macroApplication match {
          case Apply(TypeApply(Select(Select(This(TypeName("idioms")), _), TermName("idiom")), List(applicative)), _) ⇒
            val attachments = context.macroApplication.attachments
            val Pure(pure)  = attachments.get[Pure].get
            val App(app)    = attachments.get[App].get
            return (applicative.asInstanceOf[c.universe.TypeTree],
                    pure.asInstanceOf[c.universe.Tree],
                    app.asInstanceOf[c.universe.Tree])
          case _ ⇒
        }
      }
      c.abort(c.enclosingPosition, "Idiom brackets outside of idiom block")
    }

    val (applicative, pure, app) = applicativeContext

    import c.universe._

    def expandBrackets(expr: Tree): Tree = {
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

  private def resolveApplicative(c: Context)(tpe: c.Type) = {
    import c.universe._

    val typeref = TypeRef(NoPrefix, typeOf[Applicative[Option]].typeSymbol, List(tpe))

    try {
      val instance = c.inferImplicitValue(typeref)

      val pure = Select(instance, TermName("pure"))
      val app  = Select(instance, TermName("app"))

      (Pure(pure), App(app))
    } catch {
      case e: TypecheckException ⇒
        c.abort(c.enclosingPosition, s"Unable to find $typeref instance in implicit scope")
    }
  }
}

package idioms {
  private[idioms] case class Pure(tree: Any)
  private[idioms] case class App (tree: Any)
}