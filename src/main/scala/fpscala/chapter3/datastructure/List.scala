package fpscala.chapter3.datastructure

import scala.annotation.tailrec

//+A means: covariant
//sealed means all implementations should be done in this class
sealed trait List[+A]

//Nothing is a subtype of all types
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

//Companions Objects
object List {

  //Variadic functions = accepts 0 or more arguments
  //Variadic function returns a Seq type.
  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))


  def getTail[A](list: List[A]) = list match {
    case Nil => sys.error("getTail on empty list")
    case Cons(x, xs) => xs
  }

  def setHead[A](element: A, list: List[A]): List[A] = list match {
    case Nil => sys.error("setHead on empty list")
    case Cons(x, xs) => Cons(element, xs)
  }

  def getHead[A](list: List[A]): A = list match {
    case Nil => sys.error("getHead on empty list")
    case Cons(x, xs) => x
  }

  def drop[A](l: List[A], n: Int): List[A] = if (n <= 0) l else drop(List.getTail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case Cons(x, xs) => f(x, foldRight(xs, acc)(f))
    case Nil => acc
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((curr, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc, curr) => Cons(curr, acc))

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((curr, acc) => f(acc, curr))

  def foldRightUsingFoldLeft[A, B](as: List[A], acc: B)(f: (A, B) => B): B = foldLeft(reverse(as), acc)((curr, acc) => f(acc, curr))

  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((acc, curr) => Cons(curr, acc))

  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((curr, acc) => Cons(curr, acc))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def flatten2[A](a1: List[A]): List[A] = {
    foldRight(a1, List[A]())((curr, acc) => {
      if (curr.isInstanceOf[List[_]])
        append(curr.asInstanceOf[List[A]], acc)
      else
        Cons(curr, acc)
    })
  }

  def addOne(as: List[Int]) = foldRight(as, List[Int]())((curr, acc) => Cons(curr + 1, acc))

  def eachElementToString[A](as: List[A]): List[String] =
    foldRight(as, List[String]())((curr, acc) => Cons(curr.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())((curr, acc) => Cons(f(curr), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((curr, acc) => if (f(curr)) Cons(curr, acc) else acc)

  def flatMapWithFold[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]())((curr, acc) => append(f(curr), acc))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(curr => if (f(curr)) List(curr) else Nil)

  def addElements(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addElements(xs, ys))
    case (Cons(x, xs), Nil) => Cons(x, addElements(xs, Nil))
    case (Nil, Cons(y, ys)) => Cons(y, addElements(ys, Nil))
    case (Nil, Nil) => Nil
  }

  def addElementsToLeft(as: List[Int], bs: List[Int]): List[Int] = {
    @tailrec
    def sum(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = {
      (as, bs) match {
        case (Cons(x, xs), Cons(y, ys)) => sum(xs, ys, Cons(x + y, acc))
        case (Cons(x, xs), Nil) => sum(Nil, xs, Cons(x, acc))
        case (Nil, Cons(y, ys)) => sum(Nil, ys, Cons(y, acc))
        case (Nil, Nil) => acc
      }
    }

    reverse(sum(as, bs, List[Int]()))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case (Cons(x, xs), Nil) => Cons(f(x, null.asInstanceOf[B]), zipWith(xs, Nil)(f))
    case (Nil, Cons(y, ys)) => Cons(f(null.asInstanceOf[A], y), zipWith(Nil, ys)(f))
    case (Nil, Nil) => Nil
  }

  def hasSubsequence(list1: List[Int], list2: List[Int]): Boolean = {
    def find(rem1: List[Int], rem2: List[Int]): Boolean = (rem1, rem2) match {
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) find(xs, ys) else find(xs, list2)
      case (Cons(x, xs), Nil) => true
      case (Nil, Cons(y, ys)) => false
      case (Nil, Nil) => true
    }
    find(list1, list2)
  }

}