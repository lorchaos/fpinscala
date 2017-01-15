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

}
