package fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}

class EitherSpec extends WordSpec with Matchers {

  def Try[E >: Exception, A](a: => A): Either[E, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }


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

  "Exercise 4.7 - Sequence" when {

    "Right" in {

      val input = List(Right("a"), Right("b"))

      Either.sequence(input) should be (Right(List("a", "b")))
    }

    "Left" in {

      val input = List(Right("a"), Left("error"))
      Either.sequence(input) should be (Left("error"))
    }

    "Empty" in {
      Either.sequence(List()) should be (Right(List()))
    }
  }

  "Exercise 4.7 - Transpose" when {


    def toInt(s : String) = Try { s.toInt }

    "Right" in {

      val input = List("1", "2", "3")
      Either.traverse(input)(toInt) should be(Right(List(1, 2, 3)))
    }

    "Left" in {
      val input = List("1", "Nan", "3")
      Either.traverse(input)(toInt) shouldBe a [Left[Exception]]
    }

    "Empty" in {
      val input = List()
      Either.traverse(input)(toInt) should be(Right(List()))
    }
  }
}
