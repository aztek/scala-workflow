package scala.idioms

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class FunctorInstancesSpec extends FlatSpec with ShouldMatchers {
  behavior of "Built-in functor instances"

  "Tuples" should "work" in {
    idiom(tupleL[String]) {
      $(("foo", 10) * 2) should equal ("foo", 20)
    }
  }

  "Maps" should "work" in {
    idiom(map[String]) {
      $(Map("foo" → 10, "bar" → 5, "qux" → 2) * 2) should equal (Map("foo" → 20, "bar" → 10, "qux" → 4))
    }
  }
}
