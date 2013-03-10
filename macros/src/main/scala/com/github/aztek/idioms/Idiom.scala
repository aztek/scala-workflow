package com.github.aztek.idioms

import language.experimental.macros
import reflect.macros.Context

object Idiom {
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
      val (body, binds) = extractFunctionBody(code)
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
      (lambda, binds map (_._2))
    }

    def extractFunctionBody(code: Tree): (Tree, List[(TermName, Tree)]) = {
      def isLifted(arg: Tree) = {
        val tpe = c.typeCheck(arg, silent = true).tpe
        tpe.typeSymbol.isType && (tpe <:< appType)
      }

      def dissectLifted(arg: Tree) = {
        val name = TermName(c.freshName("arg$"))
        (Ident(name), (name -> arg))
      }

      def dissect(args: List[Tree]) =
        args.foldLeft((List.empty[Tree], List.empty[(TermName, Tree)])) {
          (dissects, arg) =>
            val (args, binds) = dissects
            if (isLifted(arg)) {
              val (newarg, bind) = dissectLifted(arg)
              (newarg :: args, bind :: binds)
            } else {
              (arg :: args, binds)
            }
        }

      code match {
        case expr if isLifted(expr) =>
          val (arg, bind) = dissectLifted(expr)
          (arg, List(bind))

        case Apply(expr, args) =>
          val (body, binds) = expr match {
            case Select(arg, method) =>
              val (body, binds) = extractFunctionBody(arg)
              (Select(body, method), binds)
            case _ =>
              extractFunctionBody(expr)
          }
          val (newargs, newbinds) = dissect(args)
          (Apply(body, newargs), binds ++ newbinds)

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
