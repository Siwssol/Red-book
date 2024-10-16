package Part1

import Stream.{consStream, unfold}

/*
The issue with the map and filter functions, or rather the List data structure is that for each application of the functions, we have to go through a list.
This means chaining multiple maps/filters would involve multiple passes of intermediate lists, in which those lists are discarded once used. It would be better to combine all
these transformations into a single pass but still retaining the high level compositional style.
Strictness and non-strictness are properties of functions:
  Non Strictness -> the function may choose not to evaluate one or more of its arguments.
  Strictness -> the function always evaluates its arguments
Unless we specify otherwise, all the function definitions will be strict.
The boolean operators && and || are non-strict functions
  -> This is because it can choose to evaluate the second argument depending on the outcome of the first argument
  -> With && we need both operands to be true to return true so if the first operand is evaluated to false, then we don't need to evaluate the second argument as the overall result will be false regardless
  -> With || we need at least one operand to be true to return true so if the first operand is evaluated to true, then we don't need to evaluate the second argument. It only evaluates if the first operand is evaluated to false
We say that a non-strict function takes its arguments by name rather than by value.
 */
object Chapter5 {

  /*
  To specify that we want unevaluated arguments, we can just pass an empty argument, represented by () => A. The function accepts zero arguments and returns an A.
  This unevaluated form of an expression is called a thunk. The callers of if2 will have to explicitly create thunks for onTrue and onFalse.
   */
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = {
    if (cond) onTrue() else onFalse()
  }
  /*
  Because this is common, there's a nicer syntax for this
  We can just write  => A. For example onTrue: => A. We don't need the () in the function call as there is no argument at all. Scala takes care of wrapping the expression in a thunk for us
  */
  def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }

  /*
    An argument that is passed unevaluated to a function will be evaluated once for each place it's referenced in the body of the function. This means by default Scala will not cache the result of evaluating an argument
   */
  def maybeTwice(b: Boolean, i: => Int): Int = if (b) i+i else 0

  /*
  If you want to cache the result, you can do so explicitly by using the lazy keyword. This makes it so the argument is only evaluated once.
   */
  def maybeTwice2(b: Boolean, i: => Int): Int = {
    lazy val j = i
    if (b) j+j else 0
  }

  def main(args: Array[String]) : Unit = {
    println(if2(2 < 22,
      () => println("a"),
      () => println("b")))
    /*
    If you run the function but add an intermediate print statement for each time i is evaluated, you can see that i is evaluated twice
     */
    println(maybeTwice(true, {println("hi"); 1 + 41}))
    /*
    But here it id only evaluated once.
     */
    println(maybeTwice2(true, {println("hi"); 1 + 41}))
  }
}

/*
We can define a new type of list that can deal with the problem of repeated evaluation. We use a lazy list or streams. Chains of transformations of streams
are fused into a single pass through the use of laziness.

Laziness allows us to separate the description of an expression from the evaluation of the expression. Streams produces a sequence of elements without running the steps of that computation.
Using this, we are able to describe a "larger" expression than we need and evaluate only a portion of it.
 */
sealed trait Stream[+A] {
  /*
  We have to force h explicitly via h()
   */
  def headOption: Option[A] = this match {
    case Empty => None
    case ConsStream(h, t) => Some(h())
  }

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case ConsStream(h, t) => Cons(h(), t().toList)
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case ConsStream(h, t) =>
        if (n <= 0) Stream()
        else if (n == 1) Stream(h())
        else ConsStream(h, () => t().take(n-1))
    }
  }

  def takeFold(n: Int): Stream[A] = {
    unfold((this, n))({
      case (ConsStream(a,b), n) => if (n > 0) Some(a(), (b(), n-1)) else None
      case _ => None})
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case ConsStream(h, t) =>
        if (n <= 0) return this
        if (n == 1) t()
        else t().drop(n-1)
    }
  }

  /*
  || is a non strict operator. This means we don't need to evaluate the rest of the stream once if p(h()) returns true. Not only does the traversal terminate early,
  as the tail is a lazy val, the tail of the stream is never evaluated at all.
   */
  def exists(p: A => Boolean): Boolean = this match {
    case ConsStream(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  /*
  This is similar to the foldRight for lists but now the combining function f is non-strict in the second parameter (A, => B). This means if f chooses not to evaluate the second parameter => B
  then the traversal is terminated early.
  z is the starting value for the accumulator and holds the result of the scan so far. F is the function that gets applied which updates the result of the scan. As it is a foldRight, it works backwards (from the right).
  This means that z also means the last element in the result.
  It will keep recursing 'if it needs to' then after reaching the base case, applies the function.
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case ConsStream(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  /*
  Like a foldRight but returns a stream of intermediate results.
  So Stream(1,2,3).scanRight(0)(_ + _)
    -> Stream(1+2+3+0, 2+3+0, 3+0, 0)
  How it works:
    We can use a foldRight to implement this. The starting value will be a tuple of the current result (z) and a Stream which is the actual accumulator for the eventual result (Stream(z)
    As we want to reuse results we use a lazy val to hold the intermediate tuple.
    Here ff is the intermediate result of applying the function in this case it's the sum. So it will hold the sum of the numbers. First it will hold 0.
    Then as it backtracks it will update the tuple.
    So it goes Constream(ff, Constream(ff, Constream(ff, Stream(z))))) The last case for the stream is Stream(z) which in this case is Stream(0)
    -> Working from the right, ff is currently 0:
      We have Constream(ff, Constream(ff, Constream(ff, Stream(0)))))
      Then it gets the 3 as working in reverse that would be the first value. So ff is at 3
      Constream(ff, Constream(ff, Constream(3 + 0, Stream(0)))))
    -> Continuing in a similar fashion, as the intermediate is stored lazily, it can also be retrieved without running the calculation again. So we get the ff value and add 2
      Constream(ff, Constream(2 + 3 + 0, Constream(3 + 0, Stream(0)))))
    -> And then we get back to the first value, following a similar fashion again. Here it the result is cached so it can be retrieved again.
      Constream(1 + 2 + 3 + 0, Constream(2 + 3 + 0, Constream(3 + 0, Stream(0)))))
      This results in Stream(6,5,3,0).
   Of course this will work for any combining function.
   */
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))( (a, b) => {
      lazy val intermediate = b
      val ff = f(a, intermediate._1)
      (ff, consStream(ff, intermediate._2))
    })._2

  def tailsScan: Stream[Stream[A]] = {
    scanRight(Empty: Stream[A])((x, y) => consStream(x, y))
  }

  /*
  Here b represents the recursive step that folds the tail of the stream. If p(a) returns true then b is never evaluated and the computation terminates early.
   */
  def existsRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) =>
      if (!p(a)) Empty
      else ConsStream(() => a, () => b))
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = {
    unfold(this)({
      case ConsStream(a,b) => if (p(a())) Some(a(), b()) else None
      case _ => None})
  }

  def headOptionRight: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a,b) => ConsStream(() => f(a), () => b))

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a,b) => if (p(a)) ConsStream(() => a, () => b) else Empty.append(b))
  }

  def append[B >: A](st: => Stream[B]) : Stream[B] = {
    foldRight(st)((a,b) => ConsStream(() => a, () => b))
  }

  def flatmap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a,b) => f(a).append(b))

  def mapFold[B](f: A => B): Stream[B] =
    unfold(this: Stream[A]) {
        case ConsStream(a, b) => Some(f(a()), b())
        case Empty => None
    }

  def zipWith[A1 >: A, B](st: Stream[A1])(f: (A1,A1) => B) : Stream[B] =
    unfold((this: Stream[A1], st)){
      case (Empty, Empty) => None
      case (ConsStream(a, b), ConsStream(aa, bb)) =>
        Some(f(a(), aa()), (b(), bb()))}

  /*
  Like zipWith but it's extended to handle unmatching sized Streams. It continues the traversal so long as either Stream has more elements.
  It generates a tuple of each corresponding element. If either stream has finished traversal but it can keep traversing, the values are filled with None.
   */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this: Stream[A], s2)) {
      case (Empty, Empty) => None
      case (Empty, ConsStream(a,b)) => Some((None, Some(a())), (Empty, b()))
      case (ConsStream(a,b), Empty) => Some((Some(a()), None), (b(), Empty))
      case (ConsStream(a, b), ConsStream(aa, bb)) =>
        Some((Some(a()), Some(aa())),(b(), bb()))
    }


  /*
  Even though filter transforms the whole stream, it does it lazily so it will terminate as soon as a match is found
   */
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  /*
  Checks if a Stream is a prefix of another
  e.g Stream(1,2,3) startsWith Stream(1,2) would be true
   */
  def startsWith[A1 >: A](s: Stream[A1]): Boolean =
    this.zipAll(s).takeWhile(x => x._2.getOrElse(None) != None).forAll(x => x._1 == x._2)

  /*
  Generates a Stream of suffixes of the input stream
  e.g Stream(1,2,3).tails = Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
   */
  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case x => x match {
        case ConsStream(a,b) => Some(unfold(x){
          case Empty => None
          case ConsStream(aa, bb) => Some(aa(), bb())
        },b())
      }
    }

  /*
  Now the hasSubsequence function can be implemented using functions that have been written. By using laziness we can compose the function from simpler components and still retain the efficiency
   */
  def hasSubsequence[A1](s: Stream[A1]): Boolean = {
    tails exists (_ startsWith s)
  }
}
case object Empty extends Stream[Nothing]
/*
Non empty stream consists of a head and tail, in which both are non-strict.
We specify them as thunks so we can control when we want to force these thunks (actually evaluate the argument), so we only evaluate the arguments when demanded.
 */
case class ConsStream[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /*
  The head and tail are cached as lazy values to avoid repeated evaluation.
  This is known as a smart constructor:
    a function for constructing a data type that ensures some additional invariant. Smart constructors typically are a lowercase version of the corresponding constructor.

  This method of caching values in the constructor is known as memoization and this is typically used in optimisation. By memoizing the by-name arguments for the head and tail
  of the Cons, this ensures that the thunk will only do its work once -> when it's first forced. Then each subsequent call will just return the cached lazy val.
   */
  def consStream[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    ConsStream(() => head, () => tail)
  }

  def ones: Stream[Int] = consStream(1, ones)

  def constant[A](a: A): Stream[A] =
    consStream(a, constant(a))

  def from(n: Int): Stream[Int] =
    consStream(n, from(n+1))

  def fibs: Stream[Int] = {
    val start = Stream(0,1)
    def fibsGo(intermediate: => Stream[Int]): Stream[Int] = {
      val a = intermediate.headOption.getOrElse(0)
      val b = intermediate.drop(1).headOption.getOrElse(0)
      val newIntermediate = Stream(b, a + b)
      consStream(a, fibsGo(newIntermediate))
    }
    fibsGo(start)
  }

  def fibsImproved: Stream[Int] = {
    def fibsGo(previous1: Int, previous2: Int): Stream[Int] = {
      consStream(previous1, fibsGo(previous2, previous1 + previous2))
    }
    fibsGo(0,1)
  }

  /*
  A general stream-building function. It's sometimes called a corecursive function. While recursive functions consumes data and terminates by recursing on smaller inputs, a corecursive function produces data and need not terminate so long as they remain productive, which just means that we can always evaluate more of the result in a finite number of time
  Corecursion:
    Starts from the base case and computes steps by using the output of one step as the input of the next one.
  Recursion:
    Same operation however it starts with the last step. You delay evaluation until you encounter a base condition.

   */
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case Some((a,s)) => consStream(a, unfold(s)(f))
      case _ => Empty
    }

  }

  def constantFold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a,a))

  def fromFold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n,n+1))

  def onesFold: Stream[Int] =
    unfold(1)(_ => Some(1,1))

  def fibsFold: Stream[Int] =
    unfold((0,1))({
      case (a,b) => Some((a, (b, a+b)))
    })


  /*
  The smart constructor for empty just returns Empty but types it as Stream[A] which is better for type inference. We can make it return the base type as we
  usually want to infer the data type (in this case Stream) as the type and not Cons or Empty.
   */
  def empty[A]: Stream[A] = Empty

  /*
  Creates a stream from multiple arguments.
   */
  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else consStream(as.head, apply(as.tail: _*))
  }

  def expensive(a: Any) = ???

  /*
  We typically want to cache the values of a Cons node, once they are forced. If we use the Cons constructor directly here, it would compute expensive(x) twice
  as the values are not saved.

  def example(): Unit = {
    val tail = ???
    val x = Cons( () => expensive(x), tail)
    val h1 = x.headOption
    val h2 = x.headOption
  }
  */


  def main(args: Array[String]): Unit = {
//    println(Stream(1,2,3))
//    println(Stream(1,2,3,4,5,6,7).take(4).toList)
//    println(Stream(1,2,3,4,5,6,7).drop(4).toList)
//    println(Stream(1,1,2,4,1,5,4,6,35,4).takeWhile(x => x <= 5).toList)
//    println(Stream(1283764,92834765,12389746,2413698).headOptionRight)
//    println(Stream(1,2,3,4,5,6,7).map(x => x*x).toList)
//    println(Stream(1,2,3).append(Stream(4,5,6)).toList)
//    println(Stream(1,2,3).flatmap(x => Stream(x*2, x*2)).toList)
//    println(Stream(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16).filter(x => (x%2) != 0).toList)
//    println(Stream(2,4,6,8,11,12,14,16).forAll(x => x%2 == 0))
/*
  When chaining the functions together in the Stream, it interleaves the computations of those functions. This means that as it is mapping the Stream, it will also apply filter to the intermediate result.
  This is because we do not instantiate the intermediate stream that results from the map. This allows us to reuse existing combinators without worrying about using more memory than needed
    -> For example in the below example, it will first apply map to the first element giving us ConsStream(11, Stream(2,3,4).map(_ + 10)).filter(_ % 2 == 0).toList.
       Now here it will then apply filter to that element as it's needed now for the filter function (since the computations are done lazily). This removes 11 since it does not match the predicate.
       It will continue to alternate between map and filter until we reach the end.
    -> Because the intermediate streams are not generated, a transformation of the stream (converting to another Stream) only requires enough working memory to store and transform the current element.
       The garbage collector can reclaim memory early once it becomes available, i.e when filter decides which elements are not needed. This cuts down the amount of memory required.
  This is much better than chaining functions with lists, which will execute the functions sequentially. This uses much more memory than needed.
 */
//    println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)
//
//    println(Stream(1,2,34,5,3,6,3,4,25,3,6).find(x => x==0))
    /*
    We can create infinite streams like this. Here this is a stream consisting of 1 and itself, which -> recursively generates an infinite stram of 1.
    However, as the functions are executed incrementally, we can actually use them on infinite streams, as the functions only inspect the portion of the stream that is currently needed to generate the demanded output.
    So it does not traverse the "entire" stream.
      -> Of course these functions will only work if it does indeed stop terminate traversal at some point.
     */
//    lazy val ones: Stream[Int] = Stream.consStream(1, ones)
//    println(ones.map(_ + 1).exists(_ % 2 == 0))
//    println(ones.takeWhile(_ == 1))
//    println(ones.forAll(_ != 1))
//
//    val twos: Stream[Int] = Stream.constant(2)
//    println(twos.map(_ + 1).exists(_ % 2 != 0))

//    println(Stream.fromFold(1).take(10).toList)
//
//    println(Stream.fibsFold.take(10).toList)
//
//    println(Stream.fibsFold.mapFold(x => x+1).takeFold(10).toList)
//
//    println(Stream(1,1,2,4,1,5,4,6,35,4).takeWhile(x => x <= 5).toList)
//    println(Stream(1,1,2,4,1,5,4,6,35,4).takeWhileFold(x => x <= 5).toList)

//    println(Stream(1,2,3,4,5).zipWith(Stream(1,2,3,4,5))(_ + _).toList)
//
//    println(Stream(1,2,3).zipAll(Stream(1,2,3,4,5)).toList)
//    println(Stream(1,2,3,4,5,6,7) startsWith Stream(1,2,3,4,5,6))

//    println(Stream(1,2,3).tails.map(x => x.toList).toList)
//    println(Stream(1,2,3,4,5,6,7,8,9).hasSubsequence(Stream(2,3,4,7,6)))
//    /*
//    Here due to the laziness we can use it on infinite streams
//     */
//    println(from(1).hasSubsequence(Stream(1,2,3)))

    println(Stream(1,4,2,5,3,6,3,4,2).tails.map(x => x.toList).toList)

  }
}

