package fpinscala.laziness

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {

  "Exercise 5.1 - toList" when {

    "Make sure evaluation is lazily done" in {

      def noisy(v: String) = {
        println("Evaluating " + v)
        v
      }

      val input = Stream.cons(noisy("a"), Stream.cons(noisy("b"), Stream.empty))

      println("Starting conversion")

      input.toList shouldBe List("a", "b")
    }
  }

  "Exercise 5.2 - take" when {

    "Regular case" in {
      val take = Stream("a", "b", "c").take(2)

      // find out about stream comparison
      take.toList should be(List("a", "b"))
    }

    "Less items" in {
      val take = Stream("a", "b").take(3)

      // find out about stream comparison
      take.toList should be(List("a", "b"))
    }

    "Empty stream" in {
      val take = Stream.empty.take(2)

      // find out about stream comparison
      take.toList should be(List())
    }
  }

  "Exercise 5.2 - drop" when {

    "Regular scenario" in {

      val result = Stream("a", "b", "c").drop(2)

      // find out about stream comparison
      result.toList should be(List("c"))
    }

    "Less items" in {

      val result = Stream("a", "b", "c").drop(10)

      // find out about stream comparison
      result.toList should be(List())
    }

    "Empty stream" in {

      val result = Stream().drop(2)

      // find out about stream comparison
      result.toList should be(List())
    }
  }

  "Exercise 5.3 - take while" when {

    def predicate(n : Int) = n < 3

    "Regular scenario" in {

      val result = Stream(1, 2, 3 , 4 , 5).takeWhile(predicate)

      // find out about stream comparison
      result.toList should be(List(1, 2))
    }

    "Predicate never succeeds" in {

      val result = Stream(10, 11, 12).takeWhile(predicate)

      // find out about stream comparison
      result.toList should be(List())
    }

    "Predicate never fails" in {

      val result = Stream(1, 1, 1).takeWhile(predicate)

      // find out about stream comparison
      result.toList should be(List(1, 1, 1))
    }

    "Empty stream" in {

      val result = Stream().takeWhile(predicate)

      // find out about stream comparison
      result.toList should be(List())
    }
  }

  "Exercise 5.4 - forAll" when {

    def predicate(n : Int) = n < 3

    "Predicate fails" in {
      Stream(1, 2, 3 , 4, 5).forAll(predicate) shouldBe false
    }

    "Predicate never fails" in {
      Stream(1, 2).forAll(predicate) shouldBe true
    }

    "Predicate never works" in {
      Stream(7, 8).forAll(predicate) shouldBe false
    }

    "Empty stream" in {
      Stream().forAll(predicate) shouldBe true
    }
  }
}
