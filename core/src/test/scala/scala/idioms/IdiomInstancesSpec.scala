package scala.idioms

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import util.{Success, Try}

class IdiomInstancesSpec extends FlatSpec with ShouldMatchers {
  behavior of "Built-in idiom instances"

  "Options" should "work" in {
    idiom[Option] {
      $(42) should equal (Some(42))
      $(Some("abc") + "d") should equal (Some("abcd"))
      $((None: Option[Int]) * 2) should equal (None)
      $(Some(5) * Some(3)) should equal (Some(15))
      $(Some(5) * (None: Option[Int])) should equal (None)
    }
  }

  "Lists" should "work" in {
    idiom[List] {
      $(42) should equal (List(42))
      $(List(1, 2, 3) + 1) should equal (List(2, 3, 4))
      $(List("a", "b") + List("x", "y")) should equal (List("ax", "ay", "bx", "by"))
    }
  }

  "Sets" should "work" in {
    idiom[Set] {
      $(42) should equal (Set(42))
      $(Set(1, 2, 3) * 2) should equal (Set(2, 4, 6))
      $(Set(1, 2, 4) * Set(1, 2, 4)) should equal (Set(1, 2, 4, 8, 16))
    }
  }

  "Tries" should "work" in {
    idiom[Try] {
      $(42) should equal (Try(42))
      $(Try(10) * Try(4)) should equal (Success(40))
      val failure = Try(1 / 0)
      $(failure + 5) should equal (failure)
      $(failure + Try(2 * 2)) should equal (failure)
    }
  }

  "Futures" should "work" in {
    import concurrent.{Await, Future, TimeoutException}
    import concurrent.ExecutionContext.Implicits.global
    import concurrent.duration._
    idiom[Future] {
      def slowPlus(x: Int, y: Int) = { Thread.sleep(900); x + y }
      val a = Future(slowPlus(1, 3))
      val b = Future(slowPlus(2, 4))
      evaluating(Await.result($(a * b + 3), 100 millis)) should produce[TimeoutException]
      val c = Future(slowPlus(1, 3))
      val d = Future(slowPlus(2, 4))
      Await.result($(c * d + 3), 1 second) should equal (27)
    }
  }

  "Streams" should "work" in {
    idiom[Stream] {
      $(42) should equal (Stream(42))
      $(Stream(1, 2, 3) + 1) should equal (Stream(2, 3, 4))
      $(Stream("a", "b") + Stream("x", "y")) should equal (Stream("ax", "ay", "bx", "by"))
    }
  }

  "ZipStream" should "work" in {
    idiom(zipStream) {
      val a = Stream.from(1)
      $(2).take(5) should equal (Stream(2, 2, 2, 2, 2))
      $(a + 1).take(5) should equal (Stream(2, 3, 4, 5, 6))
      $(a * a).take(5) should equal (Stream(1, 4, 9, 16, 25))
    }
  }

  "Lefts" should "work" in {
    idiom(left[String]) {
      val l:  Either[Int, String] = Left(10)
      val l2: Either[String, String] = Left("5")
      val r:  Either[Int, String] = Right("foo")
      $(42) should equal (Left(42))
      $(l + 5) should equal (Left(15))
      $(r - 2) should equal (Right("foo"))
      $(l.toString + l2) should equal (Left("105"))
      $(l + r) should equal (Right("foo"))
    }
  }

  "Rights" should "work" in {
    idiom(right[String]) {
      val r:  Either[String, Int] = Right(10)
      val r2: Either[String, Int] = Right(5)
      val l:  Either[String, Boolean] = Left("foo")
      $(42) should equal (Right(42))
      $(r + 2)  should equal(Right(12))
      $(l || false) should equal(Left("foo"))
      $(r + r2) should equal (Right(15))
      $((r > 5) || l)  should equal (Left("foo"))
    }
  }

  "Ids" should "work" in {
    // it does nothing, actually
    idiom(id) {
      $(1 + 2) should equal (1 + 2)
    }
  }

  "Partial functions" should "work" in {
    idiom(partialFunction[Int]) {
      val justFoo = $("foo")
      justFoo(42) should equal ("foo")

      val foo: PartialFunction[Int, String] = {
        case 1 ⇒ "one"
        case 2 ⇒ "two"
      }

      val fooBang = $(foo + "!")

      fooBang(2) should equal ("two!")
      evaluating(fooBang(3)) should produce[MatchError]

      val bar: PartialFunction[Int, String] = {
        case 2 ⇒ "deux"
        case 3 ⇒ "trois"
      }

      val qux = $(foo + bar)

      qux(2) should equal ("twodeux")
      evaluating(qux(1)) should produce[MatchError]
    }
  }

  "Functions" should "work" in {
    idiom(function[String]) {
      val chars   = (s: String) ⇒ s.length
      val letters = (s: String) ⇒ s.count(_.isLetter)

      val nonletters = $(chars - letters)
      nonletters("R2-D2") should equal (3)

      val weird = $(chars * 2)
      weird("C-3PO") should equal (10)

      val justFive = $(5)
      justFive("anything") should equal (5)
    }
  }

  "Functions of two arguments" should "work" in {
    idiom(function2[String, Char]) {
      val append = (s: String) ⇒ (c: Char) ⇒ s + c
      val count  = (s: String) ⇒ (c: Char) ⇒ s.count(_ == c)

      val foo = $(append + "!")
      foo("R2-D2")('2') should equal ("R2-D22!")

      val bar = $(append + count.toString)
      bar("R2-D2")('2') should equal ("R2-D222")

      val justTrue = $(true)
      justTrue("anything")('X') should equal (true)
    }
  }
}
