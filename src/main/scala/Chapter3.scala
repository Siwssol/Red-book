sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  /*
    Sum values of a list
   */
  def sum(ints: List[Int]) : Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  /*
    Returns product of elements in the list
   */
  def product(ints: List[Double]) : Double = ints match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x + product(xs)
  }
  // Sum and product are similar in terms of the structure, and so we can generalise it using a helper function

  /*
    Sets a base case value and executes an expression over the values in the list
   */
  def foldRight[A,B] (as: List[A], z: B) (f: (A,B) => B) : B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }
  /*
  List(1,2,3,4,5)
  Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  f(1, foldRight(
   */

  /*
    Now sum can just call the helper function
   */
  def sum2(l: List[Int]) : Int = {
    foldRight(l, 0)((x,y) => x+y)
  }

  /*
  Here (_ * _) means (x,y) => x * y
   */
  def product2(l: List[Double]) : Double = {
    foldRight(l, 1.0)(_ * _)
  }

  
  /*
    Appends two lists together into a single list
   */
  def append[A](a1: List[A], a2: List[A]) : List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  /*
    The * in A* means it accepts a variable number of arguments of type A
   */
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /*
    Return the list excluding the last element of that list i.e removes the last element of the list
   */
  def init[A](l: List[A]) : List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  /*
    Matches a list according to these patterns
   */
  def match31 (ints: List[Int]) : Int = {
    ints match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
  }

  /*
   Removes first element of a list
   */
  def tail[A](ints: List[A]) : List[A] = {
    ints match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  /*
   Removes first n elements from a list
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n-1)
  }

  /*
    Remove elements as long as they match a certain predicate
    Another solution:
      def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        l match {
          case Cons(h, t) => if (f(h)) dropWhile(t, f)
          case _ => l
        }
      }
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, _) =>
        if (!f(x)) l
        else dropWhile(tail(l), f)
    }
  }

  /*
  We can improve the type inference of the dropWhile function by splitting up the arguments. In the previous example
  we had to specify the type of the argument in the function even though we would know the type from the first argument

  Here the function is curried, by splitting it into argument groups.
    dropWhileImproved(xs) returns a function which is then called with argument f

  This then assists with type inference as now you don't need to annotate the type.
  By having argument groups, type information flows from left to right, so the first argument group fixes the type parameter A to be
  the type of the elements in the list that's passed in.
  Then you don't need to specify the type in subsequent groups
  */
  def dropWhileImproved[A](l: List[A])(f: A => Boolean) : List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, _) =>
        if (!f(x)) l
        else dropWhileImproved(tail(l))(f)
    }
  }


  /*
  Sets the first element of a list to a different element
   */
  def setHead[A] (newVal: A, ints: List[A]) : List[A] = {
    if (newVal == null) ints
    else ints match {
      case Nil => Cons(newVal, Nil)
      case Cons(_, xs) => Cons(newVal, xs)
    }
  }


  /*
    excercise 3.1 (correct):
      match31(List(1,2,3,4,5) ->
        It matches 3rd case Cons(x, Cons(y, Cons(3, Cons(4, _)))) with x = 1 and y = 2 so result = 3
   */
  def main(args: Array[String]) : Unit = {
    println(dropWhile(List(1,2,3,4,5), (x: Int) => x < 4))
    println(dropWhileImproved(List(1,2,3,4,5))(x => x < 4))
    println(sum2(List(1,2,3,4,5)))
    println(product2(List(1,2,3,4,5)))
    println(length(List(1,2,3,4,5)))
  }
}

object Chapter3 {


  def main(args: Array[String]) : Unit = {

  }

}
