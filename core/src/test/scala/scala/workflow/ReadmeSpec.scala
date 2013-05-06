package scala.workflow

import language.higherKinds
import language.postfixOps

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
  }

  "Examples from 'Idiom hierarchy'" should "be correct" in {
    val intTuple = new Idiom[({type λ[α] = (Int, α)})#λ] {
      def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (lhs, f(rhs)) }
      def point[A](a: ⇒ A) = (0, a)
      def app[A, B](ff: (Int, A ⇒ B)) = {
        case (i, a) ⇒
          val (i2, f) = ff
          (i + i2, f(a))
      }
    }

    idiom(intTuple) {
      val foo = (42, "foo")
      val bar = (13, "bar")
      $(foo + "bar") should equal (42, "foobar")
      $("qux") should equal (0, "qux")
      $(foo + bar) should equal (55, "foobar")
    }
  }

  "Example from 'How does it work?' section" should "be correct" in {
    idiom(option) {
      $(2 * 3 + Some(10) * Some(5)) should equal (option.app(option.map((x$1: Int) ⇒ (x$2: Int) ⇒ 2 * 3 + x$1 * x$2)(Some(10)))(Some(5)))
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
      idiom(function[Env] $ option) {
        case Var(x) ⇒ fetch(x)
        case Val(value) ⇒ $(value)
        case Add(x, y) ⇒ $(eval(x) + eval(y))
      }

    eval(testExpr1)(env) should equal (Some(48))
    eval(testExpr2)(env) should equal (None)
  }

  trait Cell[T] {
    def ! : T
    def := (value: T) { throw new UnsupportedOperationException }
  }

  val frp = new Idiom[Cell] {
    def point[A](a: ⇒ A) = new Cell[A] {
      private var value = a
      override def := (a: A) { value = a }
      def ! = value
    }
    def map[A, B](f: A ⇒ B) = a ⇒ new Cell[B] {
      def ! = f(a!)
    }
    def app[A, B](f: Cell[A ⇒ B]) = a ⇒ new Cell[B] {
      def ! = f!(a!)
    }
  }

  "FRP example" should "be correct" in {
    idiom (frp) {
      val a = $(10)
      val b = $(5)

      val c = $(a + b * 2)

      (c!) should equal (20)

      b := 7

      (c!) should equal (24)
    }
  }

  "Point-free notation examples" should "be correct" in {
    idiom(function[Char]) {
      val isLetter: Char ⇒ Boolean = _.isLetter
      val isDigit:  Char ⇒ Boolean = _.isDigit

      // Traditionally
      val isLetterOrDigit = (ch: Char) ⇒ isLetter(ch) || isDigit(ch)

      // Combinatorially
      val isLetterOrDigit2 = $(isLetter || isDigit)

      isLetterOrDigit2('X') should equal (true)
      isLetterOrDigit2('2') should equal (true)
      isLetterOrDigit2('-') should equal (false)
    }

    idiom(function[Double]) {
      val sqrt: Double ⇒ Double = x ⇒ math.sqrt(x)
      val sqr:  Double ⇒ Double = x ⇒ x * x
      val log:  Double ⇒ Double = x ⇒ math.log(x)

      // Traditionally
      val f = (x: Double) ⇒ sqrt((sqr(x) - 1) / (sqr(x) + 1))

      // Combinatorially
      val f2 = sqrt compose $((sqr - 1) / (sqr + 1))

      f2(5) should equal (f(5))

      // Traditionally
      val g = (x: Double) ⇒ (sqr(log(x)) - 1) / (sqr(log(x)) + 1)

      // Combinatorially
      val g2 = log andThen $((sqr - 1) / (sqr + 1))

      g2(10) should equal (g(10))
    }
  }
}
