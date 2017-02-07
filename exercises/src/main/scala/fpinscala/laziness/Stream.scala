package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => empty
    case (_, Empty) => empty
    case (l, Cons(h, t)) => Cons(h, () => t().take(l - 1))
  }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, t) => t
    case (_, Empty) => empty
    case (l, Cons(h, t)) => t().drop(l - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) =>
      if (p(a))
        Cons(() => a, () => b)
      else
        b)

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Some(a))


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](m: A => B): Stream[B] = foldRight(Stream[B]())((a, s) => cons(m(a), s))

  def filter(m: A => Boolean): Stream[A] = foldRight(Stream[A]())((a, s) => if (m(a)) cons(a, s) else s)

  def flatMap[B](m: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]()) { (a, s) => m(a).append(s) }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((a, acc) => cons(a, acc))
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] = {

    @annotation.tailrec
    def loop(v: Stream[A], acc: List[A]): List[A] = v match {
      case Cons(h, t) => loop(t(), acc :+ h())
      case Empty => acc
    }

    loop(this, List[A]())
  }

  def map_u[B](f: A => B): Stream[B] = unfold(this) { s =>
    s match {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def take_u(n: Int): Stream[A] = unfold((n, this)) { (a : (Int, Stream[A])) =>
   a match {
     case (0, _) | (_, Empty) => None
     case (n, Cons(h, t)) => Some(h(), (n - 1, t()))
   }
  }

  def takeWhile_u(p: A => Boolean): Stream[A] = unfold(this) { s =>

    s match {
      case Cons(h, t) if p(h()) => Some(h(), t()) // potentially inefficient, as h is evaluated twice
      case _ => None
    }
  }

}
case object Empty extends Stream[Nothing] {


}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {

}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def constant[A](a: A) : Stream[A] = Stream.cons(a, constant(a))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def from_u(n: Int): Stream[Int] = unfold(n) { s => Some((s, s + 1)) }

  val ones_u: Stream[Int] = unfold(1) { _ => Some((1, 1)) }

  def constant_u[A](a: A): Stream[A] = unfold(a) { _ => Some(a, a) }
}