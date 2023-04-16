import Stream.{unfold, _}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  //Exercise 5.1
  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  //Exercise 5.2
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 =>  t().drop(n - 1)
      case _ => this
    }
  }

  //Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  //Exercise 5.4
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }

  //Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((h,t) => if (p(h)) cons(h, t) else empty )
  }

  //Exercise 5.6
  def headOption2: Option[A] = {
    foldRight(None: Option[A])((h,_) => Some(h))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty: Stream[B])((h,t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((h,t) => if (f(h)) cons(h,t) else t)
  }

  def append[B>:A](r: Stream[B]): Stream[B] = {
    foldRight(r)((h,t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty: Stream[B])((h,t) => f(h) append t)
  }

  //Exercise 5.13
  def map2[B](f: A => B): Stream[B] = {
    unfold(this){
          case Cons(h, t) => Some( f(h()), t() )
          case _ => None
        }
  }

  def take2(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), n)  if n > 1 => Some(h(), (t(), n-1))
      case (Cons(h, t), n)  if n == 1 => Some(h(), (empty[A], n-1))
      case _ => None
    }
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B>:A](s: Stream[B], f:(B,B) => B): Stream[B] = {
    unfold((this, s)){
      case (Cons(h1,t1), Cons(h2, t2)) => Some( f(h1(), h2()), (t1(), t2())  )
      case _ => None
    }
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some( (Some(h1()), Some(h2())) , (t1(), t2()))
      case (Cons(h1, t1), _) => Some( (Some(h1()), None) , (t1(), empty[B]))
      case (_, Cons(h2, t2)) => Some( (None, Some(h2())) , (empty[A], t2()))
      case _ => None
    }
  }

  //Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean = {
    if (this.zipAll(s).takeWhile( x => x._2 != None ).filter( x => x._1 != x._2) != Empty) false else true
  }

  //Exercise 5.15
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)
  }

  //Exercise 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight( (z, Stream(z)) )( (a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

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

  //Exercise 5.8
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  //Exercise 5.9
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  //Exercise 5.10
  def fibs(n1: Int, n2: Int): Stream[Int] = {
    Stream.cons(n1, fibs(n2, n1 + n2))
  }

  //Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  //Exercise 5.12
  def constant2[A](a: A): Stream[A] = {
    unfold(a)( x => Some(x, x))
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(x => Some(x, x+1))
  }

  def fibs2(n1: Int, n2: Int): Stream[Int] = {
    unfold((n1, n2))( x => Some(x._1, (x._2, x._1 + x._2) ) )
  }

  //Test
  def main(args: Array[String]): Unit = {
    //Exercise 5.3
    println(Stream(1,2,3).takeWhile( (x: Int) => x < 4).toList)
    //Exercise 5.5
    println(Stream(1,2,3).takeWhile2( (x: Int) => x < 2).toList)
    //Exercise 5.6
    println(Stream(1,2,3).headOption2)
    println(empty.headOption2)
    //Exercise 5.10
    val s1 = Stream.fibs(0,1).take(10)
    println(s1.toList)
    //Exercise 5.12
    println(Stream.constant2(4).take(10).toList)
    println(Stream.from2(4).take(10).toList)
    println(Stream.fibs2(0,1).take(10).toList)
    //Exercise 5.13
    println(Stream.from2(4).take2(10).toList)
    println(Stream(1,2,3,4,5).takeWhile3( (x: Int) => x < 4).toList)
    //Exercise 5.14
    println(Stream(1,2,3,4,5).startsWith(Stream(1,2)))
    println(Stream(1,2,3,4,5).startsWith(Stream(2,3)))
    println(Stream(2,3).startsWith(Stream(2,3)))
  }
}

