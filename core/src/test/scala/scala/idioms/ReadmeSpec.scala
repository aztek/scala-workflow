package scala.idioms

import language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/** Auxilary spec to be sure that examples in Readme file are working */
class ReadmeSpec extends FlatSpec with ShouldMatchers {
  behavior of "Examples from Readme file"

  "Examples from 'Quick examples' section" should "be correct" in {
    idiom[Option] {
      $(Some(42) + 1) should equal (Some(43))
      $(Some(10) + Some(5) * Some(2)) should equal (Some(20))
    }

    idiom[List] {
      $(List(1, 2, 3) * 2) should equal (List(2, 4, 6))
      $(List("a", "b") + List("x", "y")) should equal (List("ax", "ay", "bx", "by"))
    }

    idiom(zipList) {
      $(List(1, 2, 3, 4) * List(2, 3, 4)) should equal (List(2, 6, 12))
    }

    idiom(function[String]) {
      val chars   = (s: String) ⇒ s.length
      val letters = (s: String) ⇒ s.count(_.isLetter)
      val nonletters = $(chars - letters)
      nonletters("R2-D2") should equal (3)
    }

    idiom(map[String]) {
      $(Map("foo" → 10, "bar" → 5) * 2) should equal (Map("foo" → 20, "bar" → 10))
    }
  }

  "Examples from 'Syntax of idioms'" should "be correct" in {
    val x = idiom[List] {
      $(List(2, 5) * List(3, 7))
    }

    val y = idiom(list) {
      $(List(2, 5) * List(3, 7))
    }

    val z = $[List](List(2, 5) * List(3, 7))

    x should equal (y)
    y should equal (z)
  }

  "Example from 'Idioms composition'" should "be correct" in {
    idiom(list $ option) {
      val xs = List(Some(2), Some(3), None)
      $(xs * 10) should equal (List(Some(20), Some(30), None))
    }
  }

  "Example from 'When are idioms useful?'" should "be correct" in {
    import java.util.Date

    type Json = String
    def parseId(json: Json): Int = ???
    def parseName(json: Json): Option[String] = ???
    def parseBirthday(json: Json): Option[Long] = ???
    def parseDept(json: Json): String = ???

    case class Person(id: Int, name: String, birthday: Date, dept: String)

    def parse(json: Json): Option[Person] =
      for {
        name ← parseName(json)
        birthday ← parseBirthday(json)
      } yield {
        val id = parseId(json)     // say, we're confident, that
        val dept = parseDept(json) // those can be extracted
        Person(id, name, new Date(birthday), dept)
      }

    def parse2(json: Json): Option[Person] =
      idiom[Option] {
        val id = parseId(json)
        val name = parseName(json)
        val birthday = parseBirthday(json)
        val dept = parseDept(json)
        $(Person(id, name, new Date(birthday), dept))
      }
  }

  sealed trait Expr
  case class Var(id: String) extends Expr
  case class Val(value: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr

  val testExpr1 = Add(Var("x"), Var("y"))
  val testExpr2 = Add(Var("x"), Var("z"))

  type Env = Map[String, Int]
  val env = Map("x" → 42, "y" → 6)

  def fetch(x: String)(env: Env): Option[Int] = env.get(x)

  "Regular eval" should "be correct" in {
    def eval(expr: Expr)(env: Env): Option[Int] =
      expr match {
        case Var(x) ⇒ fetch(x)(env)
        case Val(value) ⇒ Some(value)
        case Add(x, y) ⇒ for {
          lhs ← eval(x)(env)
          rhs ← eval(y)(env)
        } yield lhs + rhs
      }

    eval(testExpr1)(env) should equal (Some(48))
    eval(testExpr2)(env) should equal (None)
  }

  "Idiom eval" should "be correct" in {
    def eval: Expr ⇒ Env ⇒ Option[Int] =
      idiom (function[Env] $ option) {
        case Var(x) ⇒ fetch(x)
        case Val(value) ⇒ $(value)
        case Add(x, y) ⇒ $(eval(x) + eval(y))
      }

    eval(testExpr1)(env) should equal (Some(48))
    eval(testExpr2)(env) should equal (None)
  }

  "Example from 'How does it work?' section" should "be correct" in {
    idiom(option) {
      $(2 * 3 + Some(10) * Some(5)) should equal (option.app(option.app(option.pure((x$1: Int) ⇒ (x$2: Int) ⇒ 2 * 3 + x$1 * x$2))(Some(10)))(Some(5)))
    }
  }
}
