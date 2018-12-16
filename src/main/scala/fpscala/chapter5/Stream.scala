package fpscala.chapter5

trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): List[A] = this match {
    case Empty => List()
    case Cons(h, t) => if (n > 0) h() :: t().take(n - 1) else List()
  }

  def drop(n: Int): List[A] = this match {
    case Empty => List()
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else this.toList
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exist(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => if (p(a)) b else false)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) =>
    if (p(a)) Stream.cons(a, b) else Stream.empty)

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((e, s) => {
    println(e)
    Stream.cons(f(e), s)
  })

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((e, s) => if (f(e)) Stream.cons(e, s) else s)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((e, s) => Stream.cons(e, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((e, s) => f(e) append s)

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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def process(first: Int, second: Int): Stream[Int] = cons(first, process(second, first + second))
    process(0, 1)
  }
}