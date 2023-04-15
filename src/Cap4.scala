import scala.::

object Cap4 {

  def main(args: Array[String]): Unit = {

    //Exercise 4.2
    println("variance: " + variance(Seq(1.0,2.0,3.0)))
  }

  sealed trait Option[+A] {
    //Exercise 4.1
    def map[B](f: A => B): Option[B] =
      Option.this match {
        case Some(a) => Some(f(a))
        case None => None
      }

    def getOrElse[B>:A](default: => B): B =
      Option.this match {
        case Some(a) => a
        case None => default
      }

    def flatMap[B](f: A => Option[B]): Option[B] =
      Option.this.map(f).getOrElse(None)

    def orElse[B>:A](ob: => Option[B]): Option[B] =
      Option.this match {
        case Some(a) => Some(a)
        case None => ob
      }

    def filter(f: A => Boolean): Option[A] =
      Option.this match {
        case None => None
        case Some(a) => if (f(a) == true) Some(a) else None
      }

    def filter2(f: A => Boolean): Option[A] = {
      Option.this.flatMap(a => if (f(a)) Some(a) else None)
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = x => x map f

  //Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f:(A,B) => C): Option[C] =
    (a,b) match {
      case (None,_) => None
      case (_,None) => None
      case (Some(x), Some(y)) => Some(f(x,y))
    }

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  //Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val f = (h: Option[A], t: Option[List[A]]) => map2_2(h,t)( (x: A, y: List[A]) => x :: y )
    a.foldRight[Option[List[A]]](Some(Nil))(f)
  }

  //Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  //Either
  sealed trait Either[+E, +A]
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

}