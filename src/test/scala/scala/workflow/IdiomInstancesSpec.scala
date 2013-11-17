package scala.workflow

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class IdiomInstancesSpec extends FlatSpec with ShouldMatchers {
  behavior of "Built-in idiom instances"

  "ZipStream" should "work" in {
    context(zipStream) {
      val a = Stream.from(1)
      $(2).take(5) should equal (Stream(2, 2, 2, 2, 2))
      $(a + 1).take(5) should equal (Stream(2, 3, 4, 5, 6))
      $(a * a).take(5) should equal (Stream(1, 4, 9, 16, 25))
    }
  }
}
