package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that
  // the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion
      // never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the
  // stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, acc) => cons(f(a), acc))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, acc) => cons(a, acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, acc) => f(a).append(acc))
  }

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(a: Int, b: Int): Stream[Int] = Stream.cons(a, fibs(b, a + b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => empty
    }
  }

  val onesWithUnfold: Stream[Int] = unfold[Int, Any](1)(x => Some(1, x))

  def constantWithUnfold[A](a: A): Stream[A] = unfold[A, Any](a)(x => Some(a, x))

  def fromWithUnfold(n: Int): Stream[Int] = unfold[Int, Int](n)(x => Some(x, x + 1))

  def fibsWithUnfold(s: (Int, Int)): Stream[Int] = unfold[Int, (Int, Int)](s)(x => Some(x._1, (x._2, x._1 + x._2)))

}

object StreamTest {

  def toInt(str: String): Option[(Int, String)] = {
    val i = str.toInt
    if (i > 10) None
    else Some(i, (i + 1).toString)
  }

  def main(args: Array[String]): Unit = {

    val stream = Stream(1, 2, 3)

    println("toList: " + stream.toList)

    (0 to 4).foreach(i => println(s"take ${i}: " + stream.take(i).toList))

    (0 to 4).foreach(i => println(s"drop ${i}: " + stream.drop(i).toList))

    (0 to 4).foreach(i => println(s"takeWile( x <= ${i} ): " + stream.takeWhile(_ <= i).toList))

    (0 to 4).foreach(i => println(s"takeWileWithFoldRight( x <= ${i} ): " + stream.takeWhileWithFoldRight(_ <= i)
      .toList))

    (0 to 4).foreach(i => println(s"forAll( x <= ${i} ): " + stream.forAll(_ <= i)))

    println("headOption: " + empty.headOption)
    println("headOption: " + stream.headOption)

    println("map: " + stream.map(_ + 1).toList)

    println("filter: " + stream.filter(_ >= 2).toList)

    println("append: " + stream.append(stream).toList)

    println("flatMap: " + stream.flatMap(x => Stream(x - 1, x, x + 1)).toList)

    println("from(1).take(3): " + from(1).take(3).toList)

    println("fibs: " + fibs(0, 1).take(8).toList)

    println("unfold: " + unfold[Int, String]("1")(toInt).take(23).toList)

    println("ones: " + onesWithUnfold.take(3).toList)
    println("constant: " + constantWithUnfold("a").take(3).toList)
    println("from: " + fromWithUnfold(3).take(3).toList)
    println("fibs: " + fibsWithUnfold((0, 1)).take(8).toList)

  }
}