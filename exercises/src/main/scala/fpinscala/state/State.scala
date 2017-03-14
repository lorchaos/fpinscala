package fpinscala.state

import scala.collection.immutable.Stream.cons


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => (Int.MaxValue, r) // as Int.MinValue can't be converted to Int
    case (i, r) => (Math.abs(i), r)
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (i, r) if i == 0 || i == 1 => double(r) // tough luck, roll again :)
    case (i, r) => (1 / i.toDouble, r)
  }

  def double_map : Rand[Double] =
    map(nonNegativeInt)(_ * 0.5)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = nonNegativeInt(r)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {

    val (i, r) = double(rng)
    val (i2, r2) = double(r)
    val (i3, r3) = double(r2)
    ((i, i2, i3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    Stream.continually(1).take(count).foldRight((List() : List[Int], rng)) {
      (i, b) => {
        val (l, r) = b
        val (ni, nr) = r.nextInt
        (l :+ ni, nr)
      }
    }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val a = ra.apply(rng)
      val b = rb.apply(a._2)
      (f(a._1, b._1), b._2)
    }

  val intDouble_both: Rand[(Int, Double)] = both(int, double_map)

  val doubleInt_both: Rand[(Double, Int)] = both(double_map, int)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =

    rnd => {
      val initial : (List[A], RNG) = (List(), rnd)
      fs.foldRight(initial){(v, r) => {
        val p = v(r._2)
        (r._1 :+ p._1, p._2)
      }}
  }

  def ints_seq(count: Int) : Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
