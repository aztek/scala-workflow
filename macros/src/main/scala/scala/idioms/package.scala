package scala

import language.experimental.macros
import reflect.macros.Context

package object idioms {
  def $ (code: _): _ = macro optionIdiomImpl

  def optionIdiomImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val appType = c.TypeTag(typeOf[Option[_]]).tpe

    val appSelect  = Select(Ident(TermName("OptionApp")), TermName("app"))
    val pureSelect = Select(Ident(TermName("OptionApp")), TermName("pure"))

    def expandBrackets(expr: Tree): Tree = {
      def liftLambda(lambda: Tree) = Apply(pureSelect, List(lambda))

      def liftApplication(liftedLambda: Tree, args: List[Tree]) =
        args.foldLeft(liftedLambda) {
          (tree, arg) => Apply(Apply(appSelect, List(tree)), List(arg))
        }

      val (lambda, args) = composeLiftableLambda(expr)

      liftApplication(liftLambda(lambda), args)
    }

    def composeLiftableLambda(code: Tree) = {
      val (body, binds) = extractLambdaBody(code)
      val lambda = binds.foldLeft(body) {
        (tree, bind) =>
          val (name, arg) = bind
          val tpe = c.typeCheck(arg).tpe
          val existential = tpe match {
            case TypeRef(_, _, arg :: _) => TypeTree(arg)
            case _ => c.abort(arg.pos, s"Unable to determine lifted type of $arg")
          }
          val valdef = ValDef(Modifiers(), name, existential, EmptyTree)
          Function(List(valdef), tree)
      }
      val (_, args) = binds.unzip
      (lambda, args)
    }

    def extractLambdaBody(code: Tree): (Tree, List[(TermName, Tree)]) = {
      def isLifted(arg: Tree) = c.typeCheck(arg.duplicate, silent=true).tpe <:< appType

      code match {
        case expr if isLifted(expr) =>
          val name = TermName(c.freshName("arg$"))
          (Ident(name), List(name -> expr))

        case Apply(expr, args) =>
          val (body, binds) = expr match {
            case Select(arg, method) =>
              val (body, binds) = extractLambdaBody(arg)
              (Select(body, method), binds)
            case _ =>
              extractLambdaBody(expr)
          }
          val (newargs, newbinds) = args.map(extractLambdaBody(_)).unzip
          (Apply(body, newargs), binds ++ newbinds.flatten)

        case expr =>
          (expr, Nil)
      }
    }

    expandBrackets(code)
  }

  object OptionApp extends Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def app[A, B](optF: Option[A => B]): Option[A] => Option[B] =
      optA => for (f <- optF; a <- optA) yield f(a)
  }

  trait Applicative[F[_]] {
    def pure[T](t: T): F[T]
    def app[A, B](f: F[A => B]): F[A] => F[B]
  }
}
