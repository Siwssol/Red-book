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
    Sets a base case value and executes an expression over the values in the list. They combine items in a list into another item.
    FoldRight carries out the expression from right to left
   */
  def foldRight[A,B] (as: List[A], z: B) (f: (A,B) => B) : B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /*
  like foldRight but from left to right

  UPDATE -> NEED TO FIX, SEEMS TO NOT WORK FOR EVERY INPUT
   */
  @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  /*
  Modifies each value in a list into a different value, given by a function.
   */
  def map[A,B](as: List[A])(f: A => B) : List[B] = {
    foldRight(as, Nil: List[B])((x,y) => Cons(f(x),y))
  }

  /*
  Filters a list given a predicate

  TO DO -> is there a better way?
   */
  def filter[A](as: List[A])(f: A => Boolean) : List[A] = {

    def filterGo(as: List[A], filtls: List[A])(f: A => Boolean) : List[A] = {
      as match {
        case Nil => filtls
        case Cons(x, xs) => if (f(x)) filterGo(xs, appendRight(filtls)(List(x)))(f) else filterGo(xs, filtls)(f)
      }
    }

    filterGo(as, Nil)(f)

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
    Returns the length of the list
    */
  def length[A](l: List[A]) : Int = {
    foldRight(l, 0)((_,y) => 1 + y)
  }

  /*
   Calculates sum using foldLeft
   */
  def sumLeft(l: List[Int]) : Int = {
    foldLeft(l, 0)((x,y) => x + y)
  }

  /*
    Calculates product using foldLeft
   */
  def productLeft(l: List[Double]) : Double = {
    foldLeft(l, 1.0)(_ * _)
  }

  /*
    Calculates length using foldLeft
   */
  def lengthLeft[A](l: List[A]) : Int = {
    foldLeft(l, 0)((y, _) => y + 1)
  }

  /*
    Reverses the list
   */
  def reverse[A](l: List[A]) : List[A] = {
    foldLeft(l, Nil: List[A])((x,xs) => Cons(xs,x))
  }

  /*
  TO BE COMPLETE
  def foldLeftViaFoldRight[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = {
    z
  }

  def foldRightViaFoldLeft[A,B] (as: List[A], z: B) (f: (A,B) => B) : B = {
    z
  }
  */

  /*
    Appends two lists together into a single list
   */
  def append[A](a1: List[A], a2: List[A]) : List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def appendRight[A](a1: List[A])(a2: List[A]) : List[A] = {
    foldRight(a1, a2)(Cons(_,_))
  }

  /*
    TO FIGURE OUT
  def appendLeft[A](a1: List[A])(a2: List[A]) : List[A] = {
    foldLeft(a2, a1)(Cons(_,_))
  }
  */

  /*
   Concatenates a List of lists of items (in the order they appear)
   e.g concat List(List(1,2,3), List(4,5,6))
       should return List(1,2,3,4,5,6)
       it should work in linear time and for any number of lists.

    TO FIGURE OUT -> use foldLeft since it be stack safe
   */
  def concatenate[A](ls: List[List[A]]) : List[A] = {
    @annotation.tailrec
    def concatenateGo(ls: List[List[A]], conls: List[A]) : List[A] = {
      ls match {
        case Nil => conls
        case Cons(x, xs) => concatenateGo(xs, appendRight(conls)(x))
      }
    }
    concatenateGo(ls, Nil)
  }

  /*
  Adds 1 to each value in the list
   */
  def addOneToEachValue(ls: List[Int]) : List[Int] = {
    foldRight(ls, Nil: List[Int])((x,y) => Cons(x+1, y))
  }

  /*
  Converts the values in a list of doubles into strings
   */
  def eachDoubleToString(ls: List[Double]) : List[String] = {
    foldRight(ls, Nil: List[String])((x,y) => Cons(x.toString, y))
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
    println(sum2(List(1,2,3,4,5)))
    println(lengthLeft(List(1,2,3,1,1,1,1,1,1,1)))
    println(appendRight(List(1,2,3))(List(4,5,6)))
    println(addOneToEachValue(List(1,2,3)))
    println(map(List(1,2,3))(x => x+1))
    println(filter(List(1,2,3,4,5,6,7,8,9,10,11))(x => (x % 2) == 0))

  }

}

object Chapter3 {


  def main(args: Array[String]) : Unit = {

  }

}
