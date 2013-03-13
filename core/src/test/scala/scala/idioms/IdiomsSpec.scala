package scala.idioms

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class IdiomsSpec extends FlatSpec with ShouldMatchers {
  behavior of "Idiom"

  val ten:   Option[Int] = Some(10)
  val three: Option[Int] = Some(3)
  val six:   Option[Int] = Some(6)
  val four:  Option[Int] = Some(4)
  val none:  Option[Int] = None

  idiom[Option] {
    it should "lift pure expression" in {
      $("foobar") should equal (Some("foobar"))
      $(100 - 42) should equal (Some(100 - 42))
    }

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
      val minus = (a: Int, b: Int) => a - b
      $(minus(ten, three))  should equal (Some(7))
      $(minus(three, none)) should equal (None)
    }

    it should "lift curried function application" in {
      val minus = (a: Int) => (b: Int) => a - b
      $(minus(ten)(three))  should equal (Some(7))
      $(minus(three)(none)) should equal (None)
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
      val minus = (a: Int, b: Int) => a - b
      $(minus(ten, 2))   should equal (Some(8))
      $(minus(none, 2))  should equal (None)
      $(minus(12, ten))  should equal (Some(2))
      $(minus(12, none)) should equal (None)
    }

    it should "lift partially lifted curried function application" in {
      val minus = (a: Int) => (b: Int) => a - b
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
}
