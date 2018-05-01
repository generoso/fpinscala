package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are
// writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None => None
  }

  def flatMapNoPm[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  def orElseNoPm[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => this
    case _ => None
  }

  def filterNoPm(f: A => Boolean): Option[A] = {
    map((v: A) => if (f(v)) Some(v) else None).getOrElse(None)
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it
    // equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it
      // with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // SUM ( x - m ) ^2 / n
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(sa => b.map(sb => f(sa, sb)))

    // with for-comp
    // for (aa <- a; bb <- b) yield f(aa, bb)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def s(l: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = {
      l match {
        case Nil => acc
        case None :: _ => None
        case x :: xs => s(xs, x.flatMap(sx => acc.map(sacc => sx :: sacc)))
      }
    }

    s(a, Some(List())).map(_.reverse)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(List()))((x, acc) => x.flatMap(sx => acc.map(sacc => sx :: sacc)))
  }

  def sequence3[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x.flatMap(xx => sequence3(xs).map(xxs => xx :: xxs))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))(
      (aa: A, acc: Option[List[B]]) => f(aa).flatMap(sx => acc.map(sacc => sx :: sacc)))
  }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))(
      (aa: A, acc: Option[List[B]]) => map2(f(aa), acc)(_ :: _))
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(identity)
  }

}

object OptionTest {

  val some: Option[Int] = Some(1)
  val none: Option[Int] = None

  def Try[A](v: => A): Option[A] = {
    try {
      Some(v)
    } catch {
      case e: Exception => None
    }
  }

  def main(args: Array[String]): Unit = {
    println("map: " + some.map(_ + 1))
    println("flatMap: " + some.flatMap((x: Int) => Some(x + 1)))
    println("getOrElse: " + none.getOrElse(4))
    println("orElse: " + none.orElse(Some(4)))
    println("filter: " + Some(2).filter(_ % 2 == 0))
    println("filter: " + some.filter(_ % 2 == 0))
    println("filter: " + none.filter(_ % 2 == 0))

    println("seq: " + Option.sequence(List(Some(1), Some(2), Some(3))))
    println("seq: " + Option.sequence(List(Some(1), None, Some(3))))
    println("seq: " + Option.sequence2(List(Some(1), Some(2), Some(3))))
    println("seq: " + Option.sequence2(List(Some(1), None, Some(3))))
    println("seq: " + Option.sequence3(List(Some(1), Some(2), Some(3))))
    println("seq: " + Option.sequence3(List(Some(1), None, Some(3))))

    println("traverse: " + Option.traverse[String, Int](List("1", "2", "3"))(x => Try {x.toInt}))
    println("traverse: " + Option.traverse[String, Int](List("1", "not", "3"))(x => Try {x.toInt}))

    println("traverse2: " + Option.traverse2[String, Int](List("1", "2", "3"))(x => Try {x.toInt}))
    println("traverse2: " + Option.traverse2[String, Int](List("1", "not", "3"))(x => Try {x.toInt}))

    println("seq via traverse: " + Option.sequenceViaTraverse(List(Some(1), Some(2), Some(3))))
    println("seq via traverse: " + Option.sequenceViaTraverse(List(Some(1), None, Some(3))))

  }
}