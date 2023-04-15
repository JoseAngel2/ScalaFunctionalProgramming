import scala.annotation.tailrec

object Cap3 {

  def main(args: Array[String]): Unit = {

    //Exercise 3.2
    println("Tail 1: " + tail(List()))
    println("Tail 2: " + tail(List(1.0)))
    println("Tail 3: " + tail(List("a","b","c")))

    //Exercise 3.3
    println("setHead 1: " + setHead(List(), 2))
    println("setHead 2: " + setHead(List(1.0), 2.0))
    println("setHead 3: " + setHead(List("a", "b", "c"), "d"))

    //Exercise 3.6
    println("init: " + init(List(1.0, 2.0, 2.0, 3.0, 3.0)))

    //Exercise 3.8
    println("length: " + length(List(1.0, 2.0, 2.0, 3.0, 3.0)))

    //Exercise 3.9
    println("foldLeft: " + foldLeft(List(1.0, 2.0, 2.0, 3.0, 3.0), 0.0)(_ + _))

    //Exercise 3.11
    println("length2: " + length2(List(1.0, 2.0, 2.0, 3.0, 3.0)))

    //Exercise 3.12
    println("reverse: " + reverse(List(1.0, 2.0, 2.0, 3.0, 3.0)))
    println("reverse2: " + reverse2(List(1.0, 2.0, 2.0, 3.0, 3.0)))

    //Exercise 3.15
    println("concat: " + concat(List(List(1.0, 2.0, 2.0, 3.0, 3.0), List(1.0, 2.0)) ))

    //Exercise 3.16
    println("addN: " + addN(List(1, 2, 2, 3, 3), 2) )

    //Exercise 3.17
    println("toString: " + toString(List(1.0, 2.0, 2.0, 3.0, 3.0)))

    //Exercise 3.18
    println("map: " + map(List(1.0, 2.0, 3.0))(i => i * 2))

    //Exercise 3.19
    println("filter: " + filter(List(1.0, 2.0, 3.0))(i => i <= 2.0))

    //Exercise 3.20
    println("flatMap: " + flatMap(List(1.0, 2.0,  3.0))(i => List(i,i))  )

    //Exercise 3.21
    println("filter2: " + filter2(List(1.0, 2.0,  3.0))(i => i <= 2.0)  )

    //Exercise 3.22
    println("addLists2: " + addLists2(List(1, 2), List(3,4)))

    //Exercise 3.23
    println("zipWith: " + zipWith(List(1, 2), List(3, 4), (x: Int, y: Int) => x + y))

    //Exercise 3.24
    println("hasSubsequence: " + hasSubsequence(List(1,2,3), List(1,2)))
    println("hasSubsequence: " + hasSubsequence(List(1,2,3), List(2,4)))

  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object List {
    def apply[A](as: A*): List[A] = {
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }
  }


  def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs,z)(f))
    }

  //Exercise 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

  //Exercise 3.3
  def setHead[A](l: List[A], a:A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, x) => Cons(a, x)
    }

  //Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  //Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean) : List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  //Exercise 3.6
  def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }
  }

  //Exercise 3.8
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,y) => 1 + y)

  //Exercise 3.9
  def head[A](l: List[A]): A =
    l match {
      case Cons(t, _) => t
    }

  //Exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def foldLeftAux[A, B](as: List[A], z: B, acc: B)(f: (B, A) => B): B = {
      if (length(as) == 0) acc
      else foldLeftAux(tail(as), z, f(acc, head(as)))(f)
    }

    foldLeftAux(as, z, z)(f)
  }

  /*
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B, f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h), f)
  }
   */

  //Exercise 3.11
  def sum(l: List[Int]): Int = {
    foldLeft(l,0)(_+_)
  }

  def product(l: List[Double]): Double = {
    foldLeft(l, 1.0) (_*_)
  }

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((x,y) => 1 + x)
  }

  //Exercise 3.12
  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(l: List[A], acc: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h,Nil) =>Cons(h, acc)
        case Cons(h,t) => go(t, Cons(h, acc))
      }
    }
    go(l, Nil: List[A])
  }

  def reverse2[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((x,y)=>Cons(y,x))
  }

  //Exercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_,_))
  }

  //Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])(append)

  //Exercise 3.16
  def addN(l: List[Int], n: Int): List[Int] =
    foldRight(l, Nil: List[Int])((h,t) => Cons(h+n, t))

  //Exercise 3.17
  def toString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h: Double,t: List[String]) => Cons(h.toString, t))

  //Exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h: A, t: List[B]) => Cons(f(h), t))

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])( (h, t) => if(f(h) == true) Cons(h, t) else t)
  }

  //Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])( (h:A, t: List[B]) => append(f(h), t) )

  //Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)( (x: A) => if(f(x) == false) Nil: List[A] else Cons(x, Nil))
  }

  //Exercise 3.22
  def addLists(as1: List[Int], as2: List[Int]): List[List[Int]] = {
    def go(l1: List[Int], l2: List[Int], acc: List[List[Int]]): List[List[Int]] = {
      if (length(l1) == 0 || length(l2) == 0) acc
      else go(tail(l1), tail(l2), append(acc, List(List(head(l1), head(l2))))
      )
    }
    go(as1, as2, Nil: List[List[Int]])
  }

  def addLists2(as1: List[Int], as2: List[Int]): List[Int] = {
    def go(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = {
      if (length(l1) == 0 || length(l2) == 0) acc
      else go(tail(l1), tail(l2), append(acc, List(head(l1) + head(l2)))
      )
    }

    go(as1, as2, Nil: List[Int])
  }

  //Exercise 3.23
  def zipWith[A](l1: List[A], l2: List[A], f:(A,A) => A): List[A] = {
    @annotation.tailrec
    def go(x1: List[A], x2: List[A], f:(A,A) => A, acc: List[A]): List[A] = {
      if (length(x1) == 0 || length(x2) == 0) acc
      else go(tail(x1), tail(x2), f, append(acc, List(f(head(x1), head(x2)))))
    }
    go(l1,l2,f,Nil:List[A])
  }

  //Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_,Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) hasSubsequence(t1,t2) else hasSubsequence(t1, sub)
    }
  }

}