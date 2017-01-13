package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(r) => Right(f(r))
   case Left(l) => Left(l)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(r) => f(r)
   case Left(l) => Left(l)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(r) => Right(r)
   case Left(l) => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
   case (Right(aa), Right(ab)) => Right(f(aa, ab))
   case (Left(e), _ ) => Left(e)
   case (_, Left(e)) => Left(e)
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sys.error("todo")

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {

    @annotation.tailrec
    def loop(values: List[Either[E, A]], acc: List[A]) : Either[E, List[A]] = values match {
      case Nil => Right(acc)
      case Right(r) :: xs => loop(xs, acc :+ r)
      case Left(e) :: _ => Left(e)
    }

    loop(es, List())
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}