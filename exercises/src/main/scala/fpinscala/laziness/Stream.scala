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

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h, t), Cons(hh, tt)) => if (h() == hh()) t().startsWith(tt()) else false
    case (empty, Cons(hh, tt)) => false
    case (_, empty) => true
  }

  def startsWithNoRec[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile {
      case (opt1, opt2) => opt2.isDefined
    }.forAll {
      case (opt1, opt2) => opt1 == opt2
    }
  }

  // 5.13

  def mapWithUnfold[B](f: A => B): Stream[B] = {
    unfold[B, Stream[A]](this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeWithUnfold(n: Int): Stream[A] = {
    unfold[A, (Int, Stream[A])](n, this) {
      case (n, Cons(h, t)) if n == 1 => Some(h(), (0, empty[A]))
      case (n, Cons(h, t)) if n > 1 => Some(h(), (n - 1, t()))
      case _ => None
    }
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = {
    unfold[A, Stream[A]](this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  // stops as soon as one of the streams stops producing
  def zipWith[C, B](s2: Stream[C])(f: (A, C) => B): Stream[B] = {
    unfold((this, s2)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((f(h(), hh()), (t(), tt())))
      case _ => None
    }
  }

  // continues untill one of the streams is producing
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((Some(h()), Some(hh())), (t(), tt()))
      case (empty, Cons(hh, tt)) => Some((None, Some(hh())), (empty, tt()))
      case (Cons(h, t), empty) => Some((Some(h()), None), (t(), empty))
      case _ => None
    }
  }
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

  // ---------- unfold ------------

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => empty
    }
  }

  val onesWithUnfold: Stream[Int] = unfold[Int, Any](1)(Some(1, _)) // no state needed

  def constantWithUnfold[A](a: A): Stream[A] = unfold[A, Any](a)(Some(a, _)) // no state needed

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

    println("map: " + stream.mapWithUnfold(_ + 1).toList)
    println("from(1).take(3): " + from(1).takeWithUnfold(3).toList)
    println("from(1).takeWhile(_<=5): " + from(1).takeWhileWithUnfold(_ <= 5).toList)
    println("zipWith: " + stream.zipWith(stream)((x, y) => "v: " + (x + y)).toList)
    println("zipAll: " + stream.zipAll(stream.take(2)).toList)


    val s1 = from(1).take(10)
    val s2 = from(1).take(3)
    val s3 = from(1).take(14)
    val s4 = Stream(1, 3, 4)
    println("startsWith: true => " + s1.startsWith(s2))
    println("startsWith: false => " + s1.startsWith(s3))
    println("startsWith: false => " + s1.startsWith(s4))

    println("startsWithNoRec: true => " + s1.startsWithNoRec(s2))
    println("startsWithNoRec: false => " + s1.startsWithNoRec(s3))
    println("startsWithNoRec: false => " + s1.startsWithNoRec(s4))

  }
}