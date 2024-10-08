package Part1

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
      else if (!ordered(as(n-1), as(n))) false
      else isSortedLoop(n+1)
    }
    isSortedLoop(1)
  }

  /*
    Adds two numbers together
   */
  def addTwo (a: Int, b: Int) = a + b

  def addTwoCurried (a: Int)(b: Int) = a + b

  /* partial application of functions:
   Fixing a certain number of arguments in a function to return a function of a smaller arity.
   E.g incrementone -> incrementing a value by 1 can be achieved partially as 1 is fixed.
   val incrementone = partial(1, addTwo) <- we apply addTwo with a fixed argument so we only need to supply 1 argument to the function
      (see above for implementation of addTwo)
   This means that we can have incrementOne(6) which will execute addTwo(1)(6) = 7
   */
  def partial1[A,B,C] (a: A, f: (A,B) => C) : B => C = {
    (b: B) => f(a, b)
  }

  /*
    Converting a function with n arguments into a sequence of functions, with each subsequent function having 1 less argument than the previous
    Allows reuse of functions to different applications as it enables partial application
    e.g the curried version of addTwo(a,b) = (b) => a + b
      It returns a function that takes 1 less argument (b) which then returns the sum of a and b
      You call the curried function as curriedAddTwo(a)(b)
      You can store the intermediate functions as variables, which allows you to pass the functions as arguments to other functions
   */
  def curry[A,B,C] (f: (A,B) => C) : A => (B => C) = {
    (a: A) => partial1(a, f)
  }

  /*
    The reverse transformation of currying
   */
  def uncurry[A,B,C] (f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /*
    The composition of two functions i.e f compose g
   */
  def compose[A,B,C] (f: B => C, g: A => B) : A => C = {
    (a: A) => f(g(a))
  }


  def genList(n: Int, start: Int, step: Int)(f: Int => Int): List[Int] = {

    def genListGo(cur: Int, ls: List[Int])(f: Int => Int) : List[Int] = {
      if (cur > n)
        ls
      else genListGo(cur + step, Cons(f(cur), ls))(f)
    }
    List.reverse(genListGo(start, Nil)(f))
  }

  def main(args: Array[String]) : Unit = {
    println(curry(addTwo)(1)(5))
    println(uncurry(addTwoCurried)(1,5))
    println(compose(fibonacci, factorial)(3))
  }

}
