package Part1

/*
Functional programs don't update variables or modify data structures which raises questions -> what kind of data structures can we represent in functional programming and how can we do that.
A functional data structure is operated on using only pure functions -> which then means by definition all functional data structures are immutable.
  -> recall a pure function is a function that does not change data in place or cause any side effects.

traits represent abstract interfaces that may optionally contain implementations of some methods.
 */

trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


/*
  An object with the same name as the data type is known as a companion object. We can put convenience functions for creating or working with values of the data type.
 */
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
    Sets a base case value and executes an expression over the values in the list. They combine items in a list using a function into another item.
    FoldRight carries out the expression from right to left
   */
  def foldRight[A,B] (as: List[A], z: B) (f: (A,B) => B) : B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /*
  like foldRight but from left to right. As fold right is not tail-recursive and will result in a StackOverflow error for large lists.
   */
  @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  /*
   Concatenates a List of lists of items (in the order they appear)
   e.g concat List(List(1,2,3), List(4,5,6))
       should return List(1,2,3,4,5,6)
       it should work in linear time and for any number of lists.
   */
  def concatenate[A](ls: List[List[A]]) : List[A] = {
    @annotation.tailrec
    def concatenateGo(ls: List[List[A]], conls: List[A]) : List[A] = {
      ls match {
        case Nil => conls
        case Cons(x, xs) => concatenateGo(xs, append(conls,x))
      }
    }

    concatenateGo(ls, Nil)
  }

  /*
  Concatenates a list of lists with foldLeft
   */
  def concatenateleft[A](ls: List[List[A]]) : List[A] = {
    foldLeft(ls, Nil: List[A])((acc, curr) => append(acc,curr))
  }

  /*
  Modifies each value in a list into a different value, given by a function.
   */
  def map[A,B](as: List[A])(f: A => B) : List[B] = {
    foldRight(as, Nil: List[B])((x,y) => Cons(f(x),y))
  }

  /*
  Like map but the function returns a list that's inserted in the final resulting list
   */
  def flatmap[A,B](as: List[A])(f: A => List[B]): List[B] = {

    def flatmapGo(ls: List[A], fls: List[B])(f: A => List[B]) : List[B] = {
      ls match {
        case Nil => fls
        case Cons(x, xs) => flatmapGo(xs, append(fls, f(x)))(f)
      }
    }

    flatmapGo(as, Nil)(f)
  }

  /*
  Joins corresponding elements of two lists together based on a function
   */
  def zipWith[A,B] (l1: List[A], l2: List[A])(f: (A,A) => B) : List[B] = {

    @annotation.tailrec
    def zipWithGo(l1: List[A], l2: List[A], acc: List[B])(f: (A,A) => B) : List[B] = {
      (l1, l2) match {
        case (_, Nil) => acc
        case (Nil, _) => acc
        case (Cons(x,xs), Cons(y,ys)) => zipWithGo(xs, ys, appendRight(acc)(List(f(x,y))))(f)
      }
    }
    zipWithGo(l1, l2, Nil)(f)
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
  Filter but implemented using flatmap
   */
  def filterflatmap[A] (as: List[A])(f: A => Boolean) : List[A] = {
    flatmap(as)(i => if (f(i)) List(i) else Nil)
  }


  /*
  Check if list is empty or not
   */
  def isEmpty[A](ls: List[A]) : Boolean = {
    ls match {
      case Nil => true
      case _ => false
    }
  }

  /*
  Determines if a sequence contains a particular subsequence
   */

  def hasSubsequence[A] (sup: List[A], sub: List[A]) : Boolean = {

    /*
    Similar structure to zip, however here we keep track of a boolean value of whether each corresponding element matches or not.
    We can return false early if there is no match.

    TO DO:
      It looks like this whole function can be simplified.
     */
    def hasSubsequenceGo(sup: List[A], sub: List[A]) : Boolean = {
      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, _) => true
        case (Cons(x,xs), Cons(y,ys)) => if (x == y) hasSubsequenceGo(xs, ys) else false
      }
    }

    /*
    First we have to go through sequence to find match of the first element of subsequence
     */
    sup match {
      case Nil => isEmpty(sub)
      case Cons(x, xs) => sub match {
        case Nil => true
        /*
        If we find a match we then we have found the start of the potential subsequence
        We can then check each corresponding value of the sequences.
        If not we just move on to the next value of the sequence
         */
        case Cons(y, ys) => if (x==y) hasSubsequenceGo(xs, ys) else hasSubsequence(xs, sub)
      }
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


  def foldLeftViaFoldRight[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = {
    foldRight(reverse(as), z)((acc, curr) => f(curr, acc))
  }

  /*
  Maybe there's another way?
   */
  def foldRightViaFoldLeft[A,B] (as: List[A], z: B) (f: (A,B) => B) : B = {
    foldLeft(reverse(as), z)((curr, acc) => f(acc, curr))
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

  def appendRight[A](a1: List[A])(a2: List[A]) : List[A] = {
    foldRight(a1, a2)(Cons(_,_))
  }

  def appendLeft[A](a1: List[A])(a2: List[A]) : List[A] = {
    foldLeft(reverse(a1), a2)((acc, curr) => Cons(curr, acc))
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

  def addLists(l1: List[Int], l2: List[Int]) : List[Int] = {

    def addListsGo(l1: List[Int], l2: List[Int], acc: List[Int]) : List[Int] = {
      (l1, l2) match {
        case (_, Nil) => acc
        case (Nil, _) => acc
        case (Cons(x,xs),Cons(y,ys)) => addListsGo(xs, ys, append(acc,List(x+y)))
      }
    }

    addListsGo(l1, l2, Nil)

  }


  /*
    The * in A* means it accepts a variable number of arguments of type A
   */
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /*
    Return the list excluding the last element of that list i.e removes the last element of the list. This can't be done in constant time (and use data sharing) due to the nature of a list. It is essentially a singly linked list and since we only have access to the head element,
    we have to copy values until we reach the tail.
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
   Since the structures are immutable, how do we perform operations that would add/remove items from a list?
    -> This is done by returning a new list, however we can still reuse existing data (to not use up so much memory). For example if we want to remove the first element of a list, we don't need to create another copy of the list.
       We can just reuse the existing list but just reference the tail of it, as a list is made up of a head(a value) and a tail(another list) and the tail is the list without the first element.
    This reuse of data is known as data sharing.
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
    ints match {
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
//    println(sum2(List(1,2,3,4,5)))
//    println(lengthLeft(List(1,2,3,1,1,1,1,1,1,1)))
//    println(appendRight(List(1,2,3))(List(4,5,6)))
//    println(addOneToEachValue(List(1,2,3)))
//    println(map(List(1,2,3))(x => x*x))
//    println(flatmap(List(2,3,4))(x => List(x-1,x,x+1)))
//    println(filterflatmap(List(1,2,3,4,5,6,7))(x => x < 5 && x >=2))
//    println(addLists(List(1,2,3,7,8,9), List(4,5,6,15,6)))
//    println(zipWith(List(1,2,3), List(4,5,6))((x,y) => x * y))
    println(hasSubsequence(List(1,2,3,4,5,6,7,8,9,10,11,12), List(2,3,4,5,6,7,8,9)))
//    println(concatenate(List(List(4,5,2), List(1), List(1,2), List(2,2,2), List(9,8,7,6), List(1,3,4))))
  }
}

object Tree {

  /*
  Like fold for the lists but it generalises the functions for the tree.
  Similar to the fold in lists we have an empty case (represented by z)
  Here we need two functions,
    one to decide what to do with a leaf case, that will be an arity 1 function as a Leaf only holds 1 value, the value itself
    one to decide what to do with a branch case, that will be an arity 2 function as a branch holds 2 values, the left and right tree.
   */
  def fold[A,B](tr: Tree[A])(f : A => B)(g: (B,B) => B) : B = {
    tr match {
      case Leaf(x) => f(x)
      case Branch(left, right) =>
        val leftRes = fold(left)(f)(g)
        val rightRes = fold(right)(f)(g)
        g(leftRes, rightRes)
    }
  }

  def map[A,B](tr: Tree[A])(f: A => B) : Tree[B] = {

    tr match {
      case null => null
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  }

  /*
  Returns size of the tree i.e sum of the nodes (leaves and branches)
   */
  def size[A](tr: Tree[A]) : Int = {

    def sizeGo(tr: Tree[A], acc: Int) : Int = {
      tr match {
        // Nothing has size 0
        case null => acc
        // When you get to a leaf you add 1 as a leaf has size 1
        case Leaf(_) => acc + 1
        // When you get to a branch you recursively call on the left and right branch. You also increment by 1 as a branch has size 1
        case Branch(left, right) => sizeGo(left, acc) + sizeGo(right, acc) + 1
      }
    }
    sizeGo(tr, 0)
  }

  def maximum(tr: Tree[Int]) : Int = {

    def maximumGo(tr: Tree[Int], maxVal: Int) : Int = {
      tr match {
        case null => maxVal
        case Leaf(x) => x.max(maxVal)
        case Branch(left, right) => maximumGo(left, maxVal).max(maximumGo(right, maxVal))
      }
    }

    maximumGo(tr, Int.MinValue)
  }
  def depthGo[A](tr: Tree[A], maxDepth: Int) : Int = {
    tr match {
      case Leaf(_) => maxDepth
      case Branch(left, right) => depthGo(left, maxDepth + 1).max(depthGo(right, maxDepth + 1))
    }
  }

  def depth[A](tr: Tree[A]) : Int = {

    depthGo(tr, 0)
  }

  def size2[A](tr: Tree[A]) : Int = {
    fold(tr)(_ => 1)((l,r) => l + r + 1)
  }

  def maximum2(tr: Tree[Int]) : Int = {
    fold(tr)(x => x.max(Int.MinValue))((l,r) => l.max(r))
  }


  def depth2[A](tr: Tree[A]) : Int = {
    fold(tr)(_ => 0)((l,r) => (l+1).max(r+1))
  }

  def map2[A,B](tr: Tree[A])(f: A => B) : Tree[B] = {
    def xxx(l:Tree[B], r:Tree[B]): Tree[B] = Branch(l,r)
    def yyy(x: A): Tree[B] = Leaf(f(x))
    fold(tr)(yyy)(xxx)
  }

  def main(args: Array[String]) : Unit = {
    println(depth2(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))

  }

}

object Chapter3 {


  def main(args: Array[String]) : Unit = {

  }

}
