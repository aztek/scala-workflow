package com.github.aztek.idioms

import language.experimental.macros
import reflect.macros.Context

object Idiom {
  def $ (code: _): _ = macro optionIdiomImpl

  def optionIdiomImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    val appSelect  = Select(Ident(TermName("OptionApp")), TermName("app"))
    val pureSelect = Select(Ident(TermName("OptionApp")), TermName("pure"))

    println("INCOMING: " + showRaw(code))

    def expandApp(expr: c.Tree): c.Tree = {
      def lift(fun: Ident) = Apply(pureSelect, List(fun))

      def composeApp(fun: c.Tree, args: List[c.Tree]) =
        args.foldLeft(fun) {
          (tree, arg) => Apply(Apply(appSelect, List(tree)), List(arg))
        }

      expr match {
        case Apply(app @ Apply(_, _), args)   => composeApp(expandApp(app), args)
        case Apply(fun @ Ident(_), List(arg)) => composeApp(lift(fun), List(arg))
        case Apply(Select(arg, fun), args)    => composeApp(lift(Ident(fun)), arg :: args)
        case _ => code
      }
    }

    val result = expandApp(code)

    println("OUTGOING: " + showRaw(result))

    result
  }

  object OptionApp {
    def pure[A](a: A): Option[A] = Some(a)
    def app[A, B](optF: Option[A => B]): Option[A] => Option[B] =
      optA => for (f <- optF; a <- optA) yield f(a)
  }
}
