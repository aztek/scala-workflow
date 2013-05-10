package scala.workflow

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class WorkflowContextSpec extends FlatSpec with ShouldMatchers {
  behavior of "Workflow context"

  val one:   Option[Int] = Some(1)
  val two:   Option[Int] = Some(2)
  val three: Option[Int] = Some(3)
  val four:  Option[Int] = Some(4)
  val five:  Option[Int] = Some(5)
  val six:   Option[Int] = Some(6)
  val seven: Option[Int] = Some(7)
  val eight: Option[Int] = Some(8)
  val nine:  Option[Int] = Some(9)
  val ten:   Option[Int] = Some(10)
  val none:  Option[Int] = None

  val foo: Option[String] = Some("foo")
  val snone: Option[String] = None

  context[Option] {
    it should "lift object operator application" in {
      $(ten - three)  should equal (seven)
      $(three - none) should equal (None)
    }

    it should "lift method application" in {
      def minus(a: Int, b: Int) = a - b
      $(minus(ten, three))  should equal (seven)
      $(minus(three, none)) should equal (None)
    }

    it should "lift curried method application" in {
      def minus(a: Int)(b: Int) = a - b
      $(minus(ten)(three))  should equal (seven)
      $(minus(three)(none)) should equal (None)
    }

    it should "lift function application" in {
      val minus = (a: Int, b: Int) ⇒ a - b
      $(minus(ten, three))  should equal (seven)
      $(minus(three, none)) should equal (None)
    }

    it should "lift curried function application" in {
      val minus = (a: Int) ⇒ (b: Int) ⇒ a - b
      $(minus(ten)(three))  should equal (seven)
      $(minus(three)(none)) should equal (None)
    }

    it should "lift inner value method call" in {
      $(foo.reverse)   should equal (Some("oof"))
      $(snone.reverse) should equal (None)
      $(ten.toString)  should equal (Some("10"))
      $(none.toString) should equal (None)
    }

    it should "lift partially lifted object operator application" in {
      $(ten - 2)   should equal (eight)
      $(none - 2)  should equal (None)
      $(14 - ten)  should equal (four)
      $(14 - none) should equal (None)
    }

    it should "lift partially lifted method application" in {
      def minus(a: Int, b: Int) = a - b
      $(minus(ten, 2))   should equal (eight)
      $(minus(none, 2))  should equal (None)
      $(minus(12, ten))  should equal (two)
      $(minus(12, none)) should equal (None)
    }

    it should "lift partially lifted curried method application" in {
      def minus(a: Int)(b: Int) = a - b
      $(minus(ten)(2))   should equal (eight)
      $(minus(none)(2))  should equal (None)
      $(minus(12)(ten))  should equal (two)
      $(minus(12)(none)) should equal (None)
    }

    it should "lift partially lifted function application" in {
      val minus = (a: Int, b: Int) ⇒ a - b
      $(minus(ten, 2))   should equal (eight)
      $(minus(none, 2))  should equal (None)
      $(minus(12, ten))  should equal (two)
      $(minus(12, none)) should equal (None)
    }

    it should "lift partially lifted curried function application" in {
      val minus = (a: Int) ⇒ (b: Int) ⇒ a - b
      $(minus(ten)(2))   should equal (eight)
      $(minus(none)(2))  should equal (None)
      $(minus(12)(ten))  should equal (two)
      $(minus(12)(none)) should equal (None)
    }

    it should "lift compound funcall with all the arguments lifted" in {
      $(ten - (six / three))  should equal (eight)
      $(ten - (none / three)) should equal (None)
      $((ten - four) / three) should equal (two)
      $((ten - none) / three) should equal (None)
    }

    it should "lift compound funcall with some of the arguments non-lifted" in {
      $(ten - (six / 2))   should equal (seven)
      $(ten - (none / 2))  should equal (None)
      $(ten - (18 / six))  should equal (seven)
      $(ten - (18 / none)) should equal (None)
      $(2 * (ten - six))   should equal (eight)
      $(2 * (ten - none))  should equal (None)
      $((ten - six) * 3)   should equal (Some(12))
      $((ten - none) * 3)  should equal (None)
      $((ten - 4) / six)   should equal (one)
      $((ten - 4) / none)  should equal (None)
      $((1 + ten) * six)   should equal (Some(66))
      $((1 + ten) * none)  should equal (None)
    }

    it should "lift function itself" in {
      val f: Option[Int ⇒ Int] = Some(_ + 1)
      val g: Option[Int ⇒ Int] = None

      $(f(ten))  should equal (Some(11))
      $(f(none)) should equal (None)
      $(g(ten))  should equal (None)
      $(g(none)) should equal (None)
    }

    it should "lift object fields" in {
      case class Employee(name: String, boss: Option[Employee], workPlace: Option[WorkPlace])
      case class WorkPlace(cubicle: Int)

      val steve = Employee("Steve", None, Some(WorkPlace(100)))
      val john  = Employee("John", Some(steve), Some(WorkPlace(410)))
      val bob   = Employee("Bob", Some(steve), None)

      $(steve.workPlace.cubicle + 100) should equal (Some(200))
      $(bob.workPlace.cubicle   + 100) should equal (None)

      $(john.boss.workPlace.cubicle  + 100) should equal (Some(200))
      $(steve.boss.workPlace.cubicle + 100) should equal (None)
    }

    it should "lift block with a single statement" in {
      $ {
        ten - (six / 2)
      } should equal (seven)
      $ {
        ten - (none / 2)
      } should equal (None)
    }

    it should "monadically lift block with several valdefs" in {
      $ {
        val a = six
        val b = a - four
        val c = ten / b
        15 / c
      } should equal (three)
    }

    it should "monadically lift dependent subexpression" in {
      def divide(x: Double, y: Double) = if (y == 0) None else Some(x / y)
      $(divide(divide(four, one), divide(ten,  five))) should equal (two)
      $(divide(divide(four, one), divide(none, five))) should equal (none)
    }
  }

  it should "build workflow context from explicitly passed workflow instance" in {
    context(list) {
      $(List(1, 2, 3) * 2) should equal (List(2, 4, 6))
      $(List("a", "b") + List("x", "y")) should equal (List("ax", "ay", "bx", "by"))
    }
  }

  it should "resolve workflow context from passed workflow type" in {
    $[Option](ten - (six / 2)) should equal (seven)
    context[Option] {
      $[List](List(1, 2, 3) * 2) should equal (List(2, 4, 6)) // disregard enclosing context block
    }
  }

  it should "build workflow context for workflows composition" in {
    context(list $ option) {
      val xs = List(two, three, None)
      val ys = List(None, four, five)

      $(xs * 10) should equal (List(Some(20), Some(30), None))
      $(xs + ys) should equal (List(None, six, seven, None, seven, eight, None, None, None))
    }

    context(list $ option $ option) {
      val xs = List(Some(None), Some(two), None)
      val ys = List(None, Some(None), Some(five))

      $(xs + 3)  should equal (List(Some(None), Some(five), None))
      $(xs * ys) should equal (List(None, Some(None), Some(None), None, Some(None), Some(ten), None, None, None))
    }
  }
}
