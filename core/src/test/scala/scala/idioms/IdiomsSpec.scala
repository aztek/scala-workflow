package scala.idioms

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class IdiomsSpec extends FlatSpec with ShouldMatchers {
  behavior of "Idiom"

  val foo: Option[Int] = Some(10)
  val bar: Option[Int] = Some(3)
  val baz: Option[Int] = Some(6)
  val xyz: Option[Int] = Some(4)
  val qux: Option[Int] = None

  it should "lift pure expression" in {
    $("foobar") should equal (Some("foobar"))
    $(100 - 42) should equal (Some(100 - 42))
  }

  it should "lift object operator application" in {
    $(foo - bar) should equal (Some(7))
    $(bar - qux) should equal (None)
  }

  it should "lift method application" in {
    def minus(a: Int, b: Int) = a - b
    $(minus(foo, bar)) should equal (Some(7))
    $(minus(bar, qux)) should equal (None)
  }

  it should "lift curried method application" in {
    def minus(a: Int)(b: Int) = a - b
    $(minus(foo)(bar)) should equal (Some(7))
    $(minus(bar)(qux)) should equal (None)
  }

  it should "lift function application" in {
    val minus = (a: Int, b: Int) => a - b
    $(minus(foo, bar)) should equal (Some(7))
    $(minus(bar, qux)) should equal (None)
  }

  it should "lift curried function application" in {
    val minus = (a: Int) => (b: Int) => a - b
    $(minus(foo)(bar)) should equal (Some(7))
    $(minus(bar)(qux)) should equal (None)
  }

  it should "lift partially lifted object operator application" in {
    $(foo - 2)  should equal (Some(8))
    $(qux - 2)  should equal (None)
    $(14 - foo) should equal (Some(4))
    $(14 - qux) should equal (None)
  }

  it should "lift partially lifted method application" in {
    def minus(a: Int, b: Int) = a - b
    $(minus(foo, 2))  should equal (Some(8))
    $(minus(qux, 2))  should equal (None)
    $(minus(12, foo)) should equal (Some(2))
    $(minus(12, qux)) should equal (None)
  }

  it should "lift partially lifted curried method application" in {
    def minus(a: Int)(b: Int) = a - b
    $(minus(foo)(2))  should equal (Some(8))
    $(minus(qux)(2))  should equal (None)
    $(minus(12)(foo)) should equal (Some(2))
    $(minus(12)(qux)) should equal (None)
  }

  it should "lift partially lifted function application" in {
    val minus = (a: Int, b: Int) => a - b
    $(minus(foo, 2))  should equal (Some(8))
    $(minus(qux, 2))  should equal (None)
    $(minus(12, foo)) should equal (Some(2))
    $(minus(12, qux)) should equal (None)
  }

  it should "lift partially lifted curried function application" in {
    val minus = (a: Int) => (b: Int) => a - b
    $(minus(foo)(2))  should equal (Some(8))
    $(minus(qux)(2))  should equal (None)
    $(minus(12)(foo)) should equal (Some(2))
    $(minus(12)(qux)) should equal (None)
  }

  it should "lift compound funcall with all the arguments lifted" in {
    $(foo - (baz / bar)) should equal (Some(8))
    $(foo - (qux / bar)) should equal (None)
    $((foo - xyz) / bar) should equal (Some(2))
    $((foo - qux) / bar) should equal (None)
  }

  it should "lift compound funcall with some of the arguments non-lifted" in {
    $(foo - (baz / 2))  should equal (Some(7))
    $(foo - (qux / 2))  should equal (None)
    $(foo - (18 / baz)) should equal (Some(7))
    $(foo - (18 / qux)) should equal (None)
    $(2 * (foo - baz))  should equal (Some(8))
    $(2 * (foo - qux))  should equal (None)
    $((foo - baz) * 3)  should equal (Some(12))
    $((foo - qux) * 3)  should equal (None)
    $((foo - 4) / baz)  should equal (Some(1))
    $((foo - 4) / qux)  should equal (None)
    $((1 + foo) * baz)  should equal (Some(66))
    $((1 + foo) * qux)  should equal (None)
  }
}
