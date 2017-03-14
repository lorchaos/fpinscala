package fpinscala.state

import fpinscala.state.State.Rand
import org.scalatest.{Matchers, WordSpec}
import org.scalactic.Tolerance._
import org.scalatest.Assertions._


class StateSpec extends WordSpec with Matchers {

  case class Incremental(v : Int) extends RNG {
    override def nextInt = (v, Incremental(v + 1))
  }

  "Exercise 6.1 - nonNegativeInt" when {

    "Next state is returned" in {

      val state = Incremental(6)
      val (i, r) = RNG.nonNegativeInt(Incremental(6))

      i should be(6)
      r should be(Incremental(7))
    }

    "Negative value" in {
      val (i, r) = RNG.nonNegativeInt(Incremental(-6))
      i should be(6)
    }

    "Max" in {
      val (i, r) = RNG.nonNegativeInt(Incremental(Int.MaxValue))
      i should be(Int.MaxValue)
    }

    "Min" in {
      val (i, r) = RNG.nonNegativeInt(Incremental(Int.MinValue))
      i should be(Int.MaxValue)
    }
  }

  "Exercise 6.2 - double" when {

    "Basic" in {
      val (i, r) = RNG.double(Incremental(2))
      i should be(0.5)
    }

    "Zero" in {
      val (i, r) = RNG.double(Incremental(0))
      i should be(0.5)
    }

    "Big number" in {
      val (i, r) = RNG.double(Incremental(500))
      i should be(1 / 500.0)
    }

    "Negative" in {
      val (i, r) = RNG.double(Incremental(-600))
      i should be (1 / 600.0)
    }
  }

  "Exercise 6.3 - intDouble" when {

    "intDouble" in {

      val ((i, d), r) = RNG.intDouble(Incremental(1))
      i should be(1)
      d should be(0.5)
      r should be(Incremental(3))
    }

    "doubleInt" in {

      val ((d, i), r) = RNG.doubleInt(Incremental(1))
      d should be(0.5)
      i should be(3)
      r should be(Incremental(4))
    }

    "double3" in {

      val ((i, i2, i3), r) = RNG.double3(Incremental(1))
      i should be(0.5)
      i2 should equal(0.33 +- 0.1)
      i3 should be(0.25)
    }
  }

  "Exercise 6.4 - ints" when {

    "bla" in {
      val (l, r) = RNG.ints(5)(Incremental(1))

      l should be(List(1, 2, 3, 4, 5))
      r should be(Incremental(6))
    }
  }

  "Exercise 6.5 - double implmeented with map" when {

    "Basic" in {
      val result = RNG.double_map(Incremental(2))
      result._1 should be(1)
    }

    "Zero" in {
      val (i, r) = RNG.double_map(Incremental(0))
      i should be(0)
    }

    "Big number" in {
      val (i, r) = RNG.double_map(Incremental(500))
      i should be(250.0)
    }

    "Negative" in {
      val (i, r) = RNG.double_map(Incremental(600))
      i should be (300.0)
    }
  }

  "Exercise 6.6 - map2" when {

    "intDouble" in {

      val (v, r) = RNG.intDouble_both(Incremental(1))
      v._1 should be(1)
      v._2 should be(1)
      r should be(Incremental(3))
    }

    "doubleInt" in {

      val (v, r) = RNG.doubleInt_both(Incremental(1))
      v._1 should be(0.5)
      v._2 should be(2)
      r should be(Incremental(3))
    }
  }

  "Exercise 6.7 - sequence" when {

    "list" in {

      val l = RNG.ints_seq(4)(Incremental(1))
      l._1 should be(List(1, 2, 3, 4))
      l._2 should be(Incremental(5))
    }

  }
}
