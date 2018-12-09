package fpscala.chapter3.datastructure

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(x, y) => size(x) + size(y) + 1
    case Leaf(_) => 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(x, y) => maximum(x).max(maximum(y))
    case Leaf(v) => v
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Branch(x, y) => 1 + depth(x).max(depth(y))
    case Leaf(v) => 0
  }

  def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))
    case Leaf(v) => Leaf(f(v))
  }

}