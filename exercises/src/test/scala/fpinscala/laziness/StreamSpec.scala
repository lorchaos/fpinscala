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

  "Exercise 5.9 - from" when {

    "S" in {
      Stream.from(3).take(3).toList should be(List(3, 4, 5))
    }
  }

  "Exercise 5.10 - fibonacci" when {

    // TODO

  }

  "Exercise 5.11 - unfold" when {

    "Basic string decomposition" in {

      val s = Stream.unfold("abobo") {
        r =>
          r.length match {
            case 1 => Some(r.charAt(0).toInt, "")
            case 0 => None
            case x => Some(r.charAt(0).toInt, r.substring(1))
          }
      }

      s.toList should be(List(97, 98, 111, 98, 111))
    }
  }

  "Exercise 5.12 - corecursion" when {

    "From" in {
      Stream.from_u(3).take(3).toList should be(List(3, 4, 5))
    }

    "Constant" in {
      Stream.constant_u(2).take(3).toList should be(List(2, 2, 2))
    }

    "Ones" in {
      Stream.ones_u.take(3).toList should be(List(1, 1, 1))
    }

    "Fib" in {

      // TODO
    }
  }

  "Exercise 5.13 - " when {
    /**
      * Use unfold to implement map, take, takeWhile, zipWith
      * (as in chapter 3), and zipAll.
      * The zipAll function should continue the traversal as long as
      * either stream has more elements—it uses Option to indicate
      * whether each stream has been exhausted.
        def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])]
      */


    "Map" in {
      Stream(97, 98, 99).map_u{ s => s.toChar }.toList should be(List('a', 'b', 'c'))
    }

    "Take" in {
      Stream(97, 98, 99).take_u(2).toList should be(List('a', 'b'))
    }

    "TakeWhile" in {
      Stream(97, 98, 2, 1).takeWhile_u(n => n > 5).toList should be(List('a', 'b'))
    }

    "ZipWith" in {

      val s2 = Stream("a", "b", "c")

      Stream(1, 2, 3).zipWith(s2){(a, b) => a + b}.toList should be(List("1a", "2b", "3c"))
    }

    "ZipWith with less elements - a" in {

      val s2 = Stream("a")

      Stream(1, 2, 3).zipWith(s2){(a, b) => a + b}.toList should be(List("1a"))
    }

    "ZipWith with less elements - b" in {

      val s2 = Stream("a", "b", "c")

      Stream(1).zipWith(s2){(a, b) => a + b}.toList should be(List("1a"))
    }

    "ZipAll with less elements - a" in {

      val s2 = Stream("a", "b", "c")

      Stream(1).zipAll(s2).toList should be(List(
        (Some(1), Some("a")),
        (None, Some("b")),
        (None, Some("c"))))
    }

    "ZipAll with less elements - b" in {

      val s2 = Stream("a")

      Stream(1, 2, 3).zipAll(s2).toList should be(List(
        (Some(1), Some("a")),
        (Some(2), None),
        (Some(3), None)))
    }

    "ZipAll with " in {

      val s2 = Stream("a", "b", "c")

      Stream(1, 2, 3).zipAll(s2).toList should be(List(
        (Some(1), Some("a")),
        (Some(2), Some("b")),
        (Some(3), Some("c"))))
    }
  }


  "Exercise 5.14 - startsWith" when {

    "Positive" in {
      Stream(1, 2, 3).startsWith(Stream(1, 2)) should be(true)
    }

    "False" in {
      Stream(1, 2, 3).startsWith(Stream(5, 2)) should be(false)
    }

    "FailFast" in {
      Stream.from(1).startsWith(Stream.from(100)) should be(false)
    }
  }
}
