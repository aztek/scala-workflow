package scala.idioms

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import Idiom._

class IdiomaticContextSpec extends FlatSpec with ShouldMatchers {
  behavior of "Idiom"

  val ten:   Option[Int] = Some(10)
  val three: Option[Int] = Some(3)
  val six:   Option[Int] = Some(6)
  val four:  Option[Int] = Some(4)
  val none:  Option[Int] = None

  val foo: Option[String] = Some("foo")
  val snone: Option[String] = None

  idiom[Option] {
    it should "lift object operator application" in {
      $(ten - three)  should equal (Some(7))
      $(three - none) should equal (None)
    }

    it should "lift method application" in {
      def minus(a: Int, b: Int) = a - b
      $(minus(ten, three))  should equal (Some(7))
      $(minus(three, none)) should equal (None)
    }

    it should "lift curried method application" in {
      def minus(a: Int)(b: Int) = a - b
      $(minus(ten)(three))  should equal (Some(7))
      $(minus(three)(none)) should equal (None)
    }

    it should "lift function application" in {
      val minus = (a: Int, b: Int) ⇒ a - b
      $(minus(ten, three))  should equal (Some(7))
      $(minus(three, none)) should equal (None)
    }

    it should "lift curried function application" in {
      val minus = (a: Int) ⇒ (b: Int) ⇒ a - b
      $(minus(ten)(three))  should equal (Some(7))
      $(minus(three)(none)) should equal (None)
    }

    it should "lift inner value method call" in {
      $(foo.reverse)   should equal (Some("oof"))
      $(snone.reverse) should equal (None)
      $(ten.toString)  should equal (Some("10"))
      $(none.toString) should equal (None)
    }

    it should "lift partially lifted object operator application" in {
      $(ten - 2)   should equal (Some(8))
      $(none - 2)  should equal (None)
      $(14 - ten)  should equal (Some(4))
      $(14 - none) should equal (None)
    }

    it should "lift partially lifted method application" in {
      def minus(a: Int, b: Int) = a - b
      $(minus(ten, 2))   should equal (Some(8))
      $(minus(none, 2))  should equal (None)
      $(minus(12, ten))  should equal (Some(2))
      $(minus(12, none)) should equal (None)
    }

    it should "lift partially lifted curried method application" in {
      def minus(a: Int)(b: Int) = a - b
      $(minus(ten)(2))   should equal (Some(8))
      $(minus(none)(2))  should equal (None)
      $(minus(12)(ten))  should equal (Some(2))
      $(minus(12)(none)) should equal (None)
    }

    it should "lift partially lifted function application" in {
      val minus = (a: Int, b: Int) ⇒ a - b
      $(minus(ten, 2))   should equal (Some(8))
      $(minus(none, 2))  should equal (None)
      $(minus(12, ten))  should equal (Some(2))
      $(minus(12, none)) should equal (None)
    }

    it should "lift partially lifted curried function application" in {
      val minus = (a: Int) ⇒ (b: Int) ⇒ a - b
      $(minus(ten)(2))   should equal (Some(8))
      $(minus(none)(2))  should equal (None)
      $(minus(12)(ten))  should equal (Some(2))
      $(minus(12)(none)) should equal (None)
    }

    it should "lift compound funcall with all the arguments lifted" in {
      $(ten - (six / three))  should equal (Some(8))
      $(ten - (none / three)) should equal (None)
      $((ten - four) / three) should equal (Some(2))
      $((ten - none) / three) should equal (None)
    }

    it should "lift compound funcall with some of the arguments non-lifted" in {
      $(ten - (six / 2))   should equal (Some(7))
      $(ten - (none / 2))  should equal (None)
      $(ten - (18 / six))  should equal (Some(7))
      $(ten - (18 / none)) should equal (None)
      $(2 * (ten - six))   should equal (Some(8))
      $(2 * (ten - none))  should equal (None)
      $((ten - six) * 3)   should equal (Some(12))
      $((ten - none) * 3)  should equal (None)
      $((ten - 4) / six)   should equal (Some(1))
      $((ten - 4) / none)  should equal (None)
      $((1 + ten) * six)   should equal (Some(66))
      $((1 + ten) * none)  should equal (None)
    }
  }

  it should "build idiomatic context from explicitly passed idiom instance" in {
    idiom(list) {
      $(List(1, 2, 3) * 2) should equal (List(2, 4, 6))
      $(List("a", "b") + List("x", "y")) should equal (List("ax", "ay", "bx", "by"))
    }
  }

  it should "resolve idiomatic context from passed idiom type" in {
    $[Option](ten - (six / 2)) should equal (Some(7))
    idiom[Option] {
      $[List](List(1, 2, 3) * 2) should equal (List(2, 4, 6)) // disregard enclosing idiom block
    }
  }

  it should "build idiomatic context for idioms composition" in {
    idiom(list $ option) {
      val xs = List(Some(2), Some(3), None)
      val ys = List(None, Some(4), Some(5))

      $(xs * 10) should equal (List(Some(20), Some(30), None))
      $(xs + ys) should equal (List(None, Some(6), Some(7), None, Some(7), Some(8), None, None, None))
    }

    idiom(list $ option $ option) {
      val xs = List(Some(None), Some(Some(2)), None)
      val ys = List(None, Some(None), Some(Some(5)))

      $(xs + 3)  should equal (List(Some(None), Some(Some(5)), None))
      $(xs * ys) should equal (List(None, Some(None), Some(None), None, Some(None), Some(Some(10)), None, None, None))
    }
  }
}
