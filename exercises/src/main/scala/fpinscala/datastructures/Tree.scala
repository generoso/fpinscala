package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def sizeWithFold[A](tree: Tree[A]): Int = {
    fold[A, Int](tree)(_ => 1)((s1, s2) => 1 + s1 + s2)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  def maxWithFold(tree: Tree[Int]): Int = {
    fold[Int, Int](tree)(identity)((m1, m2) => m1 max m2)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def depthWithFold[A](tree: Tree[A]): Int = {
    fold[A, Int](tree)(_ => 0)((d1, d2) => 1 + (d1 max d2))
  }

  def map[A, B](tree: Tree[A])(f: (A) => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def mapWithFold[A, B](tree: Tree[A])(f: (A) => B): Tree[B] = {
    fold[A, Tree[B]](tree)((a: A) => Leaf(f(a)))((t1, t2) => Branch(t1, t2))
  }

  def print[A](tree: Tree[A]): Unit = tree match {
    case Leaf(value) => println(value)
    case Branch(left, right) => print(left); print(right)
  }

  def fold[A, B](tree: Tree[A])(fl: (A) => B)(fb: (B, B) => B): B = tree match {
    case Leaf(value) => fl(value)
    case Branch(left, right) => fb(fold(left)(fl)(fb), fold(right)(fl)(fb))
  }
}

object TreeTest {

  import Tree._

  val t0: Tree[Int] = new Branch[Int](new Leaf[Int](1), new Leaf[Int](21)) // 3 nodes
  val t1: Tree[Int] = new Branch[Int](new Leaf[Int](32), new Leaf[Int](4)) // 3 nodes
  val t2: Tree[Int] = new Branch[Int](t0, t1) // 7 nodes
  val t3: Tree[Int] = new Branch[Int](t2, new Leaf[Int](5)) // 9 nodes

  def main(args: Array[String]): Unit = {

    assertCondition(size, "size", t2, 7)
    assertCondition(size, "size", t3, 9)

    assertCondition(sizeWithFold, "sizeWithFold", t2, 7)
    assertCondition(sizeWithFold, "sizeWithFold", t3, 9)

    assertCondition(max, "max", t0, 21)
    assertCondition(max, "max", t3, 32)

    assertCondition(maxWithFold, "maxWithFold", t0, 21)
    assertCondition(maxWithFold, "maxWithFold", t3, 32)

    assertCondition(depth, "depth", t0, 1)
    assertCondition(depth, "depth", t2, 2)
    assertCondition(depth, "depth", t3, 3)

    assertCondition(depthWithFold, "depthWithFold", t0, 1)
    assertCondition(depthWithFold, "depthWithFold", t2, 2)
    assertCondition(depthWithFold, "depthWithFold", t3, 3)

    println("tree")
    print(t3)
    println("map")
    print(map(t3)(_ * 10))
    println("mapWithFold")
    print(mapWithFold(t3)(_ * 10))

  }


  def assertCondition[A](f: (Tree[A]) => Int, funcName: String, tree: Tree[A], expected: Int) = {
    val value = f(tree)
    assert(value == expected, s"$funcName should be $expected, found $value")
  }

}