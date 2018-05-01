package fpinscala.errorhandling


import scala.{Either => _, Left => _, Option => _, Right => _} // hide std library `Option` and `Either`, since we
// are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(get) => Left(get)
    case Right(get) => Right(f(get))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(get) => Left(get)
    case Right(get) => f(get)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(get) => Right(get)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }

  def map2withFor[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {a <- this; b1 <- b} yield f(a, b1)
  }
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight[Either[E, List[B]]](Right(Nil))((ea, acc) => f(ea).flatMap(a => acc.map(a :: _)))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(Nil))((ea, acc) => ea.flatMap(a => acc.map(a :: _)))
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {case e: Exception => Left(e)}

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}

}

object EitherTest {

  val left: Either[String, Int] = Left("Error!")
  val right: Either[String, Int] = Right(1)

  def main(args: Array[String]): Unit = {
    println("map: " + right.map(_ + 1))
    println("map: " + left.map(_ + 1))
    println("flatMap: " + right.flatMap((x: Int) => Right(x + 1)))
    println("flatMap: " + left.flatMap((x: Int) => Right(x + 1)))
    println("orElse: " + right.orElse(Right(3)))
    println("orElse: " + left.orElse(Right(3)))

    val as: List[Either[String, Int]] = List(Right(1), Left("Error a"))
    val bs: List[Either[String, Int]] = List(Right(2), Left("Error b"))

    for (
      a <- as;
      b <- bs
    ) println(s"map2 ${a}, ${b}: [" + a.map2(b)(_ + _) + "] [" + a.map2withFor(b)(_ + _) + "]")


    println("seq: " + Either.sequence[String, Int](List(Right(1), Right(2), Right(3))))
    println("seq: " + Either.sequence[String, Int](List(Right(1), Left("Error 1"), Right(3), Left("Error 2"))))

    println("traverse: " + Either.traverse[Exception, String, Int](List("1", "2", "3", "4"))(
      (x: String) => Either.Try {x.toInt}))
    println("traverse: " + Either.traverse[Exception, String, Int](List("1", "no2", "3", "no4"))(
      (x: String) => Either.Try {x.toInt}))

  }

}
