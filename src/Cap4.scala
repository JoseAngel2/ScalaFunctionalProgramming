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
  def traverseO[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverseO(t: List[A])(f))(_ :: _)
    }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse_1(a)(x => x)

  //Either
  sealed trait Either[+E, +A]{
    def Try[A](a: => A): Either[Exception, A] = {
      try Right(a)
      catch { case e: Exception => Left(e) }
    }
    //Exercise 4.6
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE,B] = {
      this match {
        case Left(_) => b
        case Right(a) => Right(a)
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        aa <- this
        bb <- b
      } yield f(aa,bb)
    }

  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  //Exercise 4.7
  def sequence[E, A](es: List[Either[E,A]]): Either[E, List[A]] = {
    es.foldLeft( Right(Nil: List[A]): Either[E, List[A]] )( (acc: Either[E,List[A]], x: Either[E,A]) => x.map2(acc)(_ :: _))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    sequence(as.map(f))
  }

  def traverse_2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence_1[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse_2(as)(x => x)





}