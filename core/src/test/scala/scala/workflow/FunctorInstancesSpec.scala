package scala.workflow

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class FunctorInstancesSpec extends FlatSpec with ShouldMatchers {
  behavior of "Built-in functor instances"

  "2-tuples with fixed left argument" should "work" in {
    context(tupleR[String]) {
      $(("foo", 10) * 2) should equal ("foo", 20)
    }
  }

  "2-tuples with fixed right argument" should "work" in {
    context(tupleL[Int]) {
      $(("foo", 10) + "bar") should equal ("foobar", 10)
    }
  }

  "3-tuples with fixed left argument" should "work" in {
    context(tuple3L[Boolean, Int]) {
      $(("foo", false, 10) + "bar") should equal ("foobar", false, 10)
    }
  }

  "3-tuples with fixed middle argument" should "work" in {
    context(tuple3M[String, (Int, Double)]) {
      $(("foo", 2, (10, 0.5)) * 4) should equal ("foo", 8, (10, 0.5))
    }
  }

  "3-tuples with fixed right argument" should "work" in {
    context(tuple3R[String, Unit]) {
      $(("foo", (), 10) + 2) should equal ("foo", (), 12)
    }
  }

  "Maps" should "work" in {
    context(map[String]) {
      $(Map("foo" → 10, "bar" → 5, "qux" → 2) * 2) should equal (Map("foo" → 20, "bar" → 10, "qux" → 4))
    }
  }
}
