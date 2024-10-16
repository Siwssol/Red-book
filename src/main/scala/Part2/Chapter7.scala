package Part2

/*
This chapter is all about the principles and concepts that you can use to design your own API/library in scala. First we will create a purely functional library for creating parallel and asynchronous computations.
Here we will try to keep it as high level as possible, insulating them from the nitty-gritty of how their programs will be executed

We will work iteratively:
  First we will start with a simple use case that we'd like our library to handle and then develop an interface that facilitates this use case.
  Only then we will consider the implementation of the interface.
  We oscillate between the interface and implementation as we get a better understanding of the domain and the design space through progressively more complex use cases.

First we need to understand what parallel computing is:
 */

class Chapter7 {

}

/*
We can invent a new container for the result. Par[A] for parallel and legislate the existence of the functions we need
 */

case class Par[A](a: A)

object Par{
  /*
  This takes an unevaluated A and return a computation that might evaluate in a separate thread. In a sense it creates a unit of parallelism that just wraps a single value.
   */
  def unit[A] (a: => A): Par[A] = Par(a)
  /*
  Extracts the 'result' stored in the parallel computation
   */
  def get[A](a: Par[A]): A = a match {
    case Par(a) => a
  }

  def map2[A, B, C](a: A, b: B)(f: (A, B) => C): Par[C] ={
    Par(f(a,b))
  }
}

object Chapter7 {

  /*
  A simple parallelizable computation would be the summing of integers. Typically, this is done using a fold.
  Now this traverses the entire sequence, and we are performing the addition sequentially (one at a time). However, summing can be done in any order, and most importantly we can sum multiple numbers together. We could try use a divide-and-conquer algorithm to speed things up.
   */
  def sum(ints: Seq[Int]): Int = {
    ints.foldLeft(0)(_ + _)
  }

  /*
  Here we use an indexedSeq, to allow us to split the sequence at a particular index. This essentially is like a merge sort, recursively splitting the sequence then summing each half. This implementation can be parallelized -> the two halves can be summed in parallel. So we can begin to think about the data type and functions
  that could enable parallelizing this computation.

  You can see that this function makes two recursive calls on the two halves. So ideally we'd want a data type that can contain a 'result'. That result will have a meaningful type and we want some way of extracting that result.
   */
  def sumDaC(ints: IndexedSeq[Int]): Int = {
    if (ints.size <=1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }
  }

  /*
  Using the new Par type. Here we now compute the left and right half in parallel. Then extracts both results and sums them.
  Here we can choose when we want unit to evaluate. Either we could have it evaluate it immediately in a separate logical thread, or it can simply hold onto its argument until get is called, then begin evaluation.
  If unit began evaluating its arguments concurrently, then calling get arguably breaks referential transparency. If we replace sumL and sumR with their definitions, we still get the same result but the program is no longer parallel.
  If unit starts evaluating its arguments right away, get will start waiting for the evaluation to complete.
  This means unit has a definite side effect, but only with regard to get. It returns a Par, in this case representing an asynchronous computation. But as soon as we pass that Par to get, we explicitly wait for it, exposing the side effect. We would like to delay
  calling get or avoid calling it at all.
   */
  def sumPar(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      Par.get(sumL) + Par.get(sumR)
    }

  /*
  If we avoid calling get, then sum would need to return a Par, instead of the resulting Int. This means we need a way to combine pars together.
  Here we are no longer calling unit in the recursive case, and it is unsure whether unit should accept the arguments lazily.
  For map2, we'd want to indicate that the two computations being combined are independent, and can be run in parallel. It would seem arbitrary for the order of the arguments).
  Suppose the arguments in map2 are strict:
    As Scala evaluates its arguments from left to right in the recursive call, it will create the left half of the three before moving onto the right half, which is essentially a sequential process.
  If the arguments in map2 are evaluated in parallel. This implies the left half will start executing before we even begin constructing the right half of the computation
   */
  def sumNoCallingGet(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  }

  def main(args: Array[String]): Unit = {
    println(sum(IndexedSeq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)))
    println(sumDaC(IndexedSeq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)))
    println(sumPar(IndexedSeq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)))
    println(sumNoCallingGet(IndexedSeq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)))
  }
}
