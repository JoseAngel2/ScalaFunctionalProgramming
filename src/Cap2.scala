import scala.Double.MinValue

object Cap2 {

  def main(args: Array[String]): Unit = {

    //Exercise 2.1
    println("Fib: " + fib(2))

    //Exercise 2.2
    val a1:Array[Double] = Array(1.9, 2.9, 3.4, 3.5)
    val a2:Array[Double] = Array(2.9, 1.9, 3.4, 3.5)
    println("The list is sorted: " + isSorted[Double](a1,(x: Double, y: Double) => x <= y))
    println("The list is sorted: " + isSorted[Double](a2,(x: Double, y: Double) => x <= y))

  }

  //Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibAux(n: Int, acc1: Int, acc2: Int): Int = {
      if (n == 1) acc1
      else if (n == 2) acc2
      else if (n == 3) acc1 + acc2
      else fibAux(n - 1, acc2, acc1 + acc2)
    }
    fibAux(n,0,1)
  }

  //Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n == as.length-1) true
      else if (ordered(as(n), as(n+1)) == false) false
      else loop(n+1)
    loop(0)
  }

  //Exercise 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a: A => b: B => f(a,b)
  }

  //Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    //f(a) es una función B => C, por lo que se le pasa el parámetro b
    (a: A, b: B) => f(a)(b)
  }

  //Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}