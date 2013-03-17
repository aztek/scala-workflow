package scala

import language.experimental.macros
import reflect.macros.Context

package object idioms {
  def idiom[F[_]](code: _): _ = macro idiomImpl[F]
  def idiomImpl[F[_]](c: Context)(code: c.Tree): c.Tree = code

  def $ (code: _): _ = macro bracketsImpl
  def bracketsImpl(c: Context)(code: c.Tree): c.Tree = {
    def applicativeType: c.universe.TypeTree = {
      for (context ← c.openMacros) {
        import context.universe._
        context.macroApplication match {
          case Apply(TypeApply(Select(Select(This(TypeName("idioms")), pack), TermName("idiom")), List(functorType)), _) ⇒
            return functorType.asInstanceOf[c.universe.TypeTree]
          case _ ⇒
        }
      }
      c.abort(c.enclosingPosition, "Idiom brackets outside of idiom block")
    }

    val applicativeTypeSymbol = applicativeType.tpe.typeSymbol

    import c.universe._

    val applicativeRef = Select(Select(Ident(TermName("scala")), TermName("idioms")), TermName("Applicative"))
    val pure = Select(applicativeRef, TermName("pure"))
    val app  = Select(applicativeRef, TermName("app"))

    def expandBrackets(expr: Tree): Tree = {
      def liftLambda(lambda: Tree) = {
        val tpe = c.typeCheck(lambda).tpe
        Apply(TypeApply(pure, List(Ident(applicativeTypeSymbol), TypeTree(tpe))), List(lambda))
      }

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

    def extractLambdaBody(code: Tree): (Tree, List[(TermName, Tree)]) = {
      def isLifted(arg: Tree) = c.typeCheck(arg.duplicate, silent=true).tpe.baseClasses contains applicativeTypeSymbol

      code match {
        case expr if isLifted(expr) ⇒
          val name = TermName(c.freshName("arg$"))
          (Ident(name), List(name → expr))

        case Apply(expr, args) ⇒
          val (body, binds) = expr match {
            case Select(arg, method) ⇒
              val (body, binds) = extractLambdaBody(arg)
              (Select(body, method), binds)
            case _ ⇒
              extractLambdaBody(expr)
          }
          val (newargs, newbinds) = args.map(extractLambdaBody(_)).unzip
          (Apply(body, newargs), binds ++ newbinds.flatten)

        case Select(arg, method) ⇒
          val (newarg, binds) = extractLambdaBody(arg)
          (Select(newarg, method), binds)

        case expr ⇒
          (expr, Nil)
      }
    }

    expandBrackets(code)
  }
}
