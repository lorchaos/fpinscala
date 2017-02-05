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

  "Exercise 5.5 - takeWhile with foldRight" when {

    def predicate(n : Int) = n < 3

    "Regular case" in {

      Stream(1, 2, 3, 4, 5)
        .takeWhile2(predicate)
        .toList should be(List(1, 2))
    }
  }

  "Exercise 5.6 - headOption" when {

    "Regular case" in {
      Stream(1, 2, 3).headOption should be(Some(1))
    }

    "Empty" in {
      Stream().headOption should be(None)
    }
  }

  "Exercise 5.7 - Functions using foldRight " when {

    "map" in {
      Stream(1, 2)
        .map(a => "!" + a)
        .toList should be(List("!1", "!2"))
    }

    "filter" in {
      Stream(1, 2, 3)
        .filter(a => a % 2 == 0)
        .toList should be(List(2))
    }

    "flatmap" in {
      Stream(1, 2, 3)
        .flatMap(a => Stream(a, a * 2))
        .toList should be(List(1, 2, 2, 4, 3, 6))
    }

    "append" in {
      Stream(1, 2, 3)
          .append(Stream(4, 5, 6))
        .toList should be(List(1, 2, 3, 4, 5, 6))
    }
  }

  "Exercise 5.8 - Function constant" when {

    "Numbers" in {
      Stream.constant(1).take(3).toList should be(List(1,1,1))
    }

    "Chars" in {
      Stream.constant('a').take(4).toList should be(List('a', 'a', 'a', 'a'))
    }
  }
}
