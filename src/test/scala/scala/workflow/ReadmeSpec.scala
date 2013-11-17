package scala.workflow

import language.higherKinds
import language.postfixOps

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/** Auxilary spec to be sure that examples in Readme file are working */
class ReadmeSpec extends FlatSpec with ShouldMatchers {
  behavior of "Examples from Readme file"

  def divide(x: Double, y: Double) = if (y == 0) None else Some(x / y)

  "Examples from 'Quick start' section" should "be correct" in {
    context[Option] {
      $(Some(42) + 1) should equal (Some(43))
      $(Some(10) + Some(5) * Some(2)) should equal (Some(20))
    }

    context[List] {
      $(List(1, 2, 3) * 2) should equal (List(2, 4, 6))
      $(List("a", "b") + List("x", "y")) should equal (List("ax", "ay", "bx", "by"))
    }

    context(zipList) {
      $(List(1, 2, 3, 4) * List(2, 3, 4)) should equal (List(2, 6, 12))
    }

    context(map[String]) {
      $(Map("foo" → 10, "bar" → 5) * 2) should equal (Map("foo" → 20, "bar" → 10))
    }

    context(function[String]) {
      val chars   = (s: String) ⇒ s.length
      val letters = (s: String) ⇒ s.count(_.isLetter)
      val nonletters = $(chars - letters)
      nonletters("R2-D2") should equal (3)
    }

    context[Option] {
      $ {
        val x = divide(1, 2)
        val y = divide(4, x)
        divide(y, x)
      } should equal (Some(16))
    }

    workflow[Option] {
      val x = divide(1, 2)
      val y = divide(4, x)
      divide(y, x)
    } should equal (Some(16))
  }

  "Example from 'Rules of rewriting' section" should "be correct" in {
    context(option) {
      $(2 * 3 + Some(10) * Some(5)) should equal (option.app(option.map((x$1: Int) ⇒ (x$2: Int) ⇒ 2 * 3 + x$1 * x$2)(Some(10)))(Some(5)))

      $(42) should equal (option.point(42))
      $(Some(42) + 1) should equal (option.map((x$1: Int) ⇒ x$1 + 1)(Some(42)))
      $(Some(2) * Some(3)) should equal (option.app(option.map((x$1: Int) ⇒ (x$2: Int) ⇒ x$1 * x$2)(Some(2)))(Some(3)))
      $(divide(1.5, 2)) should equal (divide(1.5, 2))
      $(divide(Some(1.5), 2)) should equal (option.bind((x$1: Double) ⇒ divide(x$1, 2))(Some(1.5)))
      $(divide(Some(1.5), Some(2))) should equal (option.bind((x$1: Double) ⇒ option.bind((x$2: Int) ⇒ divide(x$1, x$2))(Some(2)))(Some(1.5)))
      $(divide(Some(1.5), 2) + 1) should equal (option.bind((x$1: Double) ⇒ option.map((x$2: Double) ⇒ x$2 + 1)(divide(x$1, 2)))(Some(1.5)))
    }
  }

  "Examples from 'Syntax of workflows'" should "be correct" in {
    val x = context[List] {
      $(List(2, 5) * List(3, 7))
    }

    val y = context(list) {
      $(List(2, 5) * List(3, 7))
    }

    val z = $[List](List(2, 5) * List(3, 7))

    val t = workflow(list) { List(2, 5) * List(3, 7) }

    x should equal (y)
    y should equal (z)
    t should equal (z)
  }

  "Example from 'Composition of workflows'" should "be correct" in {
    context(list $ option) {
      $(List(Some(2), Some(3), None) * 10) should equal (List(Some(20), Some(30), None))
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

  "Workflow eval" should "be correct" in {
    def eval: Expr ⇒ Env ⇒ Option[Int] =
      context(function[Env] $ option) {
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
    def app[A, B](f: Cell[A ⇒ B]) = a ⇒ new Cell[B] {
      def ! = f!(a!)
    }
  }

  "FRP example" should "be correct" in {
    context(frp) {
      val a = $(10)
      val b = $(5)

      val c = $(a + b * 2)

      (c!) should equal (20)

      b := 7

      (c!) should equal (24)
    }
  }

  "Stack language interpretation" should "be correct" in {
    type Stack = List[Int]
    type State = Either[String, Stack]

    val stackLang = state[State]

    def command(f: Stack ⇒ State) = (st: State) ⇒ ((), either[String].bind(f)(st))

    def put(value: Int) = command {
      case stack ⇒ Right(value :: stack)
    }

    def dup = command {
      case head :: tail ⇒ Right(head :: head :: tail)
      case _ ⇒ Left("Stack underflow while executing `dup`")
    }

    def rot = command {
      case a :: b :: stack ⇒ Right(b :: a :: stack)
      case _ ⇒ Left("Stack underflow while executing `rot`")
    }

    def sub = command {
      case a :: b :: stack ⇒ Right((b - a) :: stack)
      case _ ⇒ Left("Stack underflow while executing `sub`")
    }

    def execute(program: State ⇒ (Unit, State)) = {
      val (_, state) = program(Right(Nil))
      state
    }

    context(stackLang) {
      val programA = $ { put(5); dup; put(7); rot; sub }
      execute(programA) should equal(Right(List(2, 5)))

      val programB = $ { put(5); dup; sub; rot; dup }
      execute(programB) should equal(Left("Stack underflow while executing `rot`"))
    }
  }

  "Point-free notation examples" should "be correct" in {
    context(function[Char]) {
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

    context(function[Double]) {
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

  "Purely functional logging" should "work" in {
    val logging = accumulator[List[String]]

    def mult(x: Int, y: Int) = (x * y, List(s"Calculating $x * $y"))

    def info(message: String) = (Unit, List(message))

    val (result, log) = workflow(logging) {
      info("Lets define a variable")
      val x = 2

      info("And calculate a square of it")
      val square = mult(x, x)

      info("Also a cube and add them together")
      val cube = mult(mult(x, x), x)
      val sum = square + cube

      info("This is all so silly")
      sum / 2
    }

    result should equal (6)
    log should equal (List("Lets define a variable",
                           "And calculate a square of it",
                           "Calculating 2 * 2",
                           "Also a cube and add them together",
                           "Calculating 2 * 2",
                           "Calculating 4 * 2",
                           "This is all so silly"))
  }
}
