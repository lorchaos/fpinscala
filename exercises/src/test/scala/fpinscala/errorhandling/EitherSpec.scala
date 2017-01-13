package fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}

class EitherSpec extends WordSpec with Matchers {

  "Exercise 4.6" when {

    val l : Either[String, Int] = Left("Bob")
    val r : Either[String, Int] = Right(7)

    "Map" in {

      def fn(i: Int) = i + 2

      l.map(fn) should be (l)
      r.map(fn) should be (Right(9))
    }

    "FlatMap" in {

      def fn[Either[EE, String]](i: Int) = Right(i.toString)

      l.flatMap(fn) should be (l)
      r.flatMap(fn) should be (Right("7"))
    }

    "orElse" in {

      val or = Right(8)

      l.orElse(or) should be (or)
      r.orElse(or) should be (r)
    }

    "map2" in {

      val v = Right(4)
      def fn(a: Int, b: Int) = a.toString() + ":" + b.toString

      l.map2(v)(fn) should be (l)
      r.map2(v)(fn) should be (Right("7:4"))
    }
  }
}
