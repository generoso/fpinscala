package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs // use _ if the argument is not used
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("can't set head of empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l // Nil or !f(x)
    }
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(a: List[A], ll: List[A]): List[A] = ll match {
      case Nil => sys.error("init on empty list")
      case Cons(x, Nil) => a
      case Cons(x, xs) => loop(Cons(x, a), xs)
    }

    @annotation.tailrec
    def reverse(v: List[A], a: List[A]): List[A] = v match {
      case Nil => a
      case Cons(x, xs) => reverse(xs, Cons(x, a))
    }

    reverse(loop(List(), l), List())
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, b) => b + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, _) => b + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((al, e) => Cons(e, al))
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b, a) => f(a, b))
  }

  def append2[A](l: List[A], l2: List[A]): List[A] = {
    foldRight(l, l2)(Cons(_, _))
  }

  def concat[A](ll: List[List[A]]): List[A] = {
    foldRight(ll, List[A]())(append2)
  }

  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((x, z) => Cons(x + 1, z))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((d, z) => Cons(d.toString, z))
  }


  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((a, lb) => Cons(f(a), lb))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((a, la) => if (f(a)) Cons(a, la) else la)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight(l, Nil: List[B])((a, lb) => append2(f(a), lb))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((a: A) => if (f(a)) List(a) else Nil)
  }

  def zipSum(l: List[Int], k: List[Int]): List[Int] = {
    def inZip(ll: List[Int], kk: List[Int], acc: List[Int]): List[Int] = ll match {
      case Nil => acc
      case Cons(llh, llt) => kk match {
        case Nil => acc
        case Cons(kkh, kkt) => Cons(llh + kkh, inZip(llt, kkt, acc))
      }
    }

    inZip(l, k, Nil: List[Int])
  }

  def zipSum1(l: List[Int], k: List[Int]): List[Int] = {
    @annotation.tailrec
    def inZip(ll: List[Int], kk: List[Int], acc: List[Int]): List[Int] = ll match {
      case Nil => acc
      case Cons(llh, llt) => kk match {
        case Nil => acc
        case Cons(kkh, kkt) => inZip(llt, kkt, Cons(llh + kkh, acc))
      }
    }

    reverse(inZip(l, k, Nil: List[Int]))
  }

  def zipWith[A, B](l: List[A], k: List[A])(f: (A, A) => B): List[B] = (l, k) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(hl, tl), Cons(hk, tk)) => Cons(f(hl, hk), zipWith(tl, tk)(f))
  }


  /*
   * These implementations are not tailrec
   */

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(hl, tl), Cons(hk, tk)) => (hl == hk) && startsWith(tl, tk)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(hl, tl), Cons(hk, tk)) => startsWith(sup, sub) || hasSubsequence(tl, sub)
  }

  /*
   * These implementations are tailerc
   */

  def startsWithTr[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(hl, tl), Cons(hk, tk)) if hl == hk => startsWithTr(tl, tk)
    case _ => false
  }

  def hasSubsequenceTr[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, tl) => hasSubsequenceTr(tl, sub)
  }
}

object ListTest {

  def main(args: Array[String]): Unit = {
    val l = List.init(List(1, 2, 3, 4, 5))
    println(l)
    println("Length: " + List.length(l))
    println("fold left len: " + List.length2(l))
    println("reverse: " + List.reverse(l))
    println("sum: " + List.foldRightViaFoldLeft(l, 0)(_ + _))
    println("append: " + List.append2(l, List(12, 13)))

    println("concat: " + List.concat(List(List(1, 2), List(3, 4), List(5, 6))))

    println("add 1: " + List.addOne(List(1, 2, 3)))

    println("double to string: " + List.doubleToString(List(1, 2, 3)))

    println("filter: " + List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))

    println("flatMap: " + List.flatMap(List(1, 2, 3))((x: Int) => List(x, x)))

    println("filter via flatMap: " + List.filterViaFlatMap(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))

    println("zipSum: " + List.zipSum1(List(1, 2, 3), List(4, 5, 6)))

    println("zipWith: " + List.zipWith(List(1, 2, 3, 5, 4), List(4, 5, 6))(_ + _))

    println("startsWith: " + List.startsWith(List(1, 2, 3, 4), List(1, 2)))

    println("startsWith: " + List.startsWith(List(1, 2, 3, 4), List(2, 3)))

    println("hasSubsequence: " + List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))

    println("startsWith: " + List.startsWithTr(List(1, 2, 3, 4), List(2, 3)))

    println("hasSubsequence: " + List.hasSubsequenceTr(List(1, 2, 3, 4), List(2, 3)))
  }
}