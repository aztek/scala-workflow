package scala.workflow

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class SemiIdiomInstancesSpec extends FlatSpec with ShouldMatchers {
  behavior of "Built-in semi-context instances"

  "ZipLists" should "work" in {
    context(zipList) {
      $(List(1, 2, 3).toString + "!") should equal (List("1!", "2!", "3!"))
      $(List(1, 2, 3, 4) * List(2, 3, 4)) should equal (List(2, 6, 12))
    }
  }
}
