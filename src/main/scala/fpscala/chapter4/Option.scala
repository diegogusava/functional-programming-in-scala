package fpscala.chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = if (this == None) default else this

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) => if (f(v)) this else None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def variance(xs: Seq[Double]): Option[Double] = {

    def mean(xs: Seq[Double]) = if (xs.isEmpty) None else Some(xs.sum / xs.length)

    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    val result = a.flatMap(aa => aa.map(v => List(v)).getOrElse(Nil))
    if (result.isEmpty || result.size != a.size) None else Some(result)
  }

  /**
    * It wont process all the elements if it finds a None element.
    * None.flatMap always will return None and it wont call sequence recursively
    */
  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap (xx => sequence_2(xs) map (xx :: _))
  }

  /**
    * It will process all elements even if it finds a None element.
    */
  def sequence_3[A](a: List[Option[A]]): Option[List[A]] = {
    val initialValue: Option[List[A]] = Some(Nil)
    val process = (curr: Option[A], acc: Option[List[A]]) => {
      curr.flatMap((v: A) => acc.map((vv: List[A]) => v :: vv))
    }
    a.foldRight[Option[List[A]]](initialValue)(process)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => f(x) flatMap (curr => traverse(xs)(f) map (acc => curr :: acc))
  }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
  }

  def Try[A](value: => A): Option[A] = {
    try {
      Some(value)
    } catch {
      case e: Exception => None
    }
  }

  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}