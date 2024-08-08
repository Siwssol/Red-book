object Chapter2 {
  def fibonacci(n: Int) : Int = {
    @annotation.tailrec
    def fibGo(n: Int, prev: Int, acc: Int) : Int = {
      if (n <= 2) acc
      else fibGo(n-1, acc, acc + prev)
    }
    if (n == 1) 0
    else fibGo(n, 0, 1)
  }

  def factorial(n: Int) : Int = {
    def factGo(n : Int, acc: Int) : Int = {
      if (n <= 0) acc
      else factGo(n-1 , acc * n)
    }
    factGo(n, 1)
  }

  def abs(n: Int) : Int = {
    if (n < 0) -n
    else n
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %dth %s number is %d"
    msg.format(n, name, f(n))
  }

  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    @annotation.tailrec
    def isSortedLoop(n: Int) : Boolean = {
      if (n == as.length) true
      else if (!ordered(as(n-1), as(n-2))) false
      else isSortedLoop(n+1)
    }
    isSortedLoop(1)
  }

  def addTwo (a: Int, b: Int) = a + b

  def addTwoCurried (a: Int)(b: Int) = a + b

  def partial1[A,B,C] (a: A, f: (A,B) => C) : B => C = {
    (b: B) => f(a, b)
  }

  def curry[A,B,C] (f: (A,B) => C) : A => (B => C) = {
    (a: A) => partial1(a, f)
  }

  def uncurry[A,B,C] (f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]) : Unit = {
    println(curry(addTwo)(1)(5))
    println(uncurry(addTwoCurried)(1,5))
    println(compose(fibonacci, factorial)(3))
  }
}
