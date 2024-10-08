package Part1

/*
This library allows us to generate randomness (pseudo-randomness). However, this uses a imperative style that relies on side effects.
We are able to write this same API using a purely functional style
 */
import Part1.State.unit

import scala.annotation.tailrec
import scala.util.Random


/*
One way to make to recover referential transparency is to make the state updates explicit i.e return the nes state alon with the value that's being generated. this leaves the old state unmodified.
 */
trait RNG {
  /*
  We give the caller control on what to do with the new state. Here now we separate the concern of computing what the next state is from the concern of communicating the new state to the rest of the program. The state is still being encapsulated meaning the caller has no idea on the implementation of RNG.
   */
  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  /*
   Here the implementation we will use is an algorithm to generate pseudo-random numbers -> Linear congruential generator.
   Some things to note:
      We generate a new seed for the next RNG state
      We generate a new number using that new seed
   Based on some research:
      0x5DEECE66DL = 25214903917 -> the multiplier(a)
      0xBL = 11 -> the increment(c)
      0xFFFFFFFFFFFFL -> modulus(m)
      We generate the new seed using the following recurrence relation:
        ((seed * multiplier) + increment) mod modulus
        Here the modulus can be computed using bitwise operations since we are dealing with powers of 2, we are just taking the n least significant bits i.e the remainder
   */
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
/*
Of course this is not limited to RNG. We can create a more general type to encapsulate state actions.
Here State is short for computation that carries some state along, or state action.
 */
case class State[S, +A](run: S => (A,S)) {
  def map[B](f: A => B): State[S, B] = {
    flatmap(i => unit(f(i)))
  }
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatmap(i => sb.flatmap(j => unit(f(i, j))))
  }
  def flatmap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

}


/*
Suppose we have a trait that contains objects bar and baz which mutate in some way. We can translate this to a purely functional API by making the explicit the transition from one state to the next.
 trait Foo {
  def bar: (Bar, Foo)
  def baz: (Int, Foo)
}

case class A() extends Foo {
  private var s: FooState = ???
  def bar: Bar
  def baz: Int
}
 */
object State {
  /*
  Now using this, we can define Rand in terms of State
   */
  type Rand[A] = State[RNG, A]

  def unit[A, S](a: A): State[S, A] = {
    State(s => (a,s))
  }

  def sequence[A, S](fs: Part1.List[State[S, A]]): State[S, Part1.List[A]] = {
    def sequenceGo(fs: Part1.List[State[S, A]], s: S): (Part1.List[A], S) = {
      fs match {
        case Part1.Nil => (Part1.Nil, s)
        case Cons(r, xs) =>
          val (a, newS) = r.run(s)
          (Cons(a, sequenceGo(xs, newS)._1), newS)
      }
    }

    State(s => sequenceGo(fs, s))
  }

  /*
 We can see that the sequence of assigning a state action to a val and run another state action using that val, it follows a similar structure to imperative programming.
 Imperative programming:
   A sequence of statements where each statement may modify the program state. So in this case the statements are just state actions which are really functions
 Functional programming:
   Programming without side effects.
*/
  /*
  The map/map2/flatmap combinators have been implemented to handle the propagation of the state from one statement to the next. However, in doing so, we have lost a bit of the imperative method.
  val ns: Rand[List[Int]] =
    int.flatmap(x =>
      int.flatmap(y =>
        ints(x).map(xs =>
          xs.map(_ % Y)))

  From this, it is not very clear what this val is doing, but since it is using map and flatmap, we can recover the imperative style using a for comprehension

  val ns: Rand[List[Int]] = for {
    x <- int
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)

  Now it is a bit clearer to see what is going on:
    We generate an integer x
    We generate an integer y
    We generate a list of x integers
    Then finally map each value to be the value modulus y

  In the background, it is doing the same thing as the map and flatmap. It looks like an imperative program that maintains some state.
  Inorder to facilitate this kind of programming with for-comprehensions, we only need two primitive State combinators:
    -> One for reading the state (get)
    -> One for writing the state (set)
  Then we can use a combinator that can modify the state in arbitrary ways.
   */

  /*
  Get simply passes the incoming state along and returns it as the value
   */
  def get[S]: State[S, S] = State(s => (s,s))

  /*
  Set constructs a new action with a new state s. The resulting action ignores the incoming state, replaces it with the new state and returns () instead of a meaningful value.
   */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  /*
  Then we can implement a combinator that can modify the state in arbitrary ways. This method returns a State action that modifies the incoming state by the function f.
  It yields Unit to indicate that it doesn't have a return value other than the state
   */
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get[S]
    _ = set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/*
Not sure how to get the machine itself.
object Machine {

  def machineUpdate: Input => Machine => Machine = (input: Input) => (state: Machine) =>
    (input, state) match {
      case (_, Machine(_, 0, _)) => state
      case (Turn, Machine(true, _, _)) => state
      case (Coin, Machine(false, _, _)) => state
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin + 1)
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(List.map(inputs)(modify[Machine] _ compose machineUpdate))
      endState = get[Machine]
    } yield (endState.coins, endState.candies)

  def main(args: Array[String]): Unit = {
    val machine = Machine(true, 5, 10)
    val inputs = List(Turn, Coin, Turn, Coin, Coin, Turn, Turn, Turn, Coin, Coin, Turn)
    println(simulateMachine(inputs))
  }
}
*/



object Chapter6 {

  def int: Rand[Int] = _.nextInt
  /*
  We can define a type alias for the RNG action data type.
  A value can be seen as a "randomly generated A", however it also transitions the RNG to a new state that can be used by another action later. Precisely, it is a state action.
  Note:
    Got to put the definition within the object for it to work/be recognised?
  */
  type Rand[+A] = RNG => (A, RNG)

  def rngNotes(): Unit = {
    val rng = new Random()
    /*
    When you call the rng.nextDouble() function twice you get two different results. In fact when you run it again you will get different results, which is expected since this is what we want from a random function.
    From a functional perspective, this indicates side effects and so breaking referential transparency as the object rng, must have some internal state updated after each invocation, which gives us the different values.
      This makes it harder to be testable/composable/parallelizable.
     */
    println(rng.nextDouble())
    println(rng.nextDouble())
    /*
    For example suppose we are testing a function that simulates the rolling of a dice. We'd expect the function to return a number from 1 to 6. However the random function has a one off error and actually returns a value from 0 to 5
    However, though it is correct 5 out of 6 times, we'd still like to reliably detect and reproduce the failure. As the problems become bigger, it is more important to reproduce the failure.
     */
    def rollDie: Int = {
      rng.nextInt(6)
    }
    println(rollDie)
    /*
    We can try to resolve this by passing in the same rng generator. However, there is still a problem with doing that. We need to make sure that the 'same' generator has been created with the same seed. That way it will have the same sequence of state
    transformations. This is also much more difficult as the implementation destroys the previous state after use. We also need to know how many times the generator has been called, which is really difficult to guarantee. We need to instead remove side effect in principle.
     */

    /*
    Here we can run the sequence of statements as many times as we want and we will get the same set of states. This makes it referentially transparent and therefore created a pure version of RNG. This is a general pattern used to make API's purely functional. We explicitly pass the computed state to the caller making the caller responsible for passing it to
    the rest of the program.
     */
    val rngN = new SimpleRNG(42)
    val (n1, rng2) = rngN.nextInt
    println(n1)
    println(rng2)

    /*
    We can turn methods such as nextInt into values of the new type Rand:
     */

    /*
    Now by turning it into a functional style, we can make a more testable dice roll.
     */
    def rollDieImproved: Rand[Int] = nonNegativeLessThan(6)

    /*
    Here we can test various RNG states to find the one that will return the erroneous result. This means we can also recreate this reliably without worrying about the state being destroyed after use.
    We can also fix this bug through functions we have implemented.
      Here we just add 1 to the result to fix the one-off error.
     */
    def rollDieFixed: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)


  }

  /*
  Whenever we use the pattern, we make the caller responsible for passing the computed next state through the rest of the program. So given a previous state it will always generate the same value it generated before.
   */
  def randomPair(rng: RNG): (Int, Int) = {
    /*
    Here i1 and i2 will be the same
     */
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }

  /*
  To generate 2 different numbers we need to use the RNG returned by the first call to nextInt to generate the second Int i.e we just call it twice each time.
   */
  def randomPairDiff(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  /*
  We can see a common pattern with each of the implementations. We are transforming one state into the next:
    They are of the form state => (A, newState) for some type A.
    Functions of this type are called state actions or state transitions. These state actions can be combines using combinators, which are higher-order functions. The combinators pass the state from one action to the next automatically.
   */
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRNG) = rng.nextInt
    if (n >= 0 && n <= Int.MaxValue) (n, newRNG)
    else nonNegativeInt(newRNG)
  }

  /*
  Generates a random double between 0 and 1
   */
  @tailrec
  def double(rng: RNG): (Double, RNG) = {
    val (n, newRNG) = rng.nextInt
    val normalisedDouble = n.toDouble/Int.MaxValue.toDouble
    if (normalisedDouble >= 0 && normalisedDouble < 1) (normalisedDouble, newRNG)
    else double(newRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((n,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (n, rng3) = nonNegativeInt(rng2)
    ((d,n), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  def ints(counts: Int)(rng: RNG): (Part1.List[Int], RNG) = {
    def intsGo(ls: Part1.List[Int], counts: Int)(rng: RNG): (Part1.List[Int], RNG) = {
      if (counts == 0) (ls, rng)
      else {
        val (n, newRNG) = nonNegativeInt(rng)
        intsGo(Part1.List.append(ls, Part1.List(n)), counts - 1)(newRNG)
      }
    }
    intsGo(Part1.Nil, counts)(rng)
  }

  /*
  We want to write combinators that let us combine Rand actions while avoiding explicitly passing along the RNG state. We end up with some sort
  of domain specific language that does all this passing for us.
  The simple RNG state transition is the unit action, which passes the RNG state without using it.
    It always returns a constant value rather tan a random value.
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /*
  There's a map function that transforms the output of a state action without modifying the state itself. This is kind of like a function composition. However, this only works for a single state transformation
   */
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /*
  If we want to combine two RNG actions into one, we need a new combinator map2 that will combine them through a binary function rather than a unary function.
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
  }

  /*
  It would be nice to pass the rng implicitly rather than explicitly. This is what flatmap does. It will pass in rng based on the function provided, but it can also not pass in the rng if it's not required
   */
  def flatmap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  /*
  Since we can implement map and map2 using flatmap, it means flatmap is more powerful than map and map2
   */
  def mapFlat[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatmap(s)(i => unit(f(i)))
  }

  def map2Flat[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatmap(ra)(i => flatmap(rb)(b => unit(f(i,b))))
  }

  /*
  Now we can implement the function without the need of an explicit rng
   */
  def nonNegativeIntLessThanFlatmap(n: Int): Rand[Int] = {
    flatmap(nonNegativeInt)(i => {
      val mod = i % n
      /*
      When it is less than, we don't need to pass in the new rng and can just return the unit action.
       */
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeIntLessThanFlatmap(n)
    })
  }

  /*
  For example we can implement nonNegativeInt using map.
    Here this reuses nonNegativeInt to generate an Int that's greater than or equal to zero and divisible by two.
   */
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)((i: Int) => i - i % 2)

  def doubleMap: Rand[Double] = {
    map(double)(i => i)
  }

  /*
  Now we can use the map2 combinator to combine arbitrary RNG state actions. So if we have an action that generates values of type A and an action to generate values of type B, then we can combine them into one action that generates
  pairs of both A and B
   */
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  /*
  Now we can use this to reimplement intDouble and doubleInt
   */
  def intDoubleMap: Rand[(Int, Double)] = {
    both(int, double)
  }

  def doubleIntMap: Rand[(Double, Int)] = {
    both(double, int)
  }

  /*
  As we can combine 2 rands together, we can also combine a whole list of them.
   */
  def sequence[A](fs: Part1.List[Rand[A]]): Rand[Part1.List[A]] = {
    def sequenceGo(fs: Part1.List[Rand[A]], ran: RNG): (Part1.List[A], RNG) = {
      fs match {
        case Part1.Nil => (Part1.Nil, ran)
        case Cons(r, xs) =>
          val (a, newR) = r(ran)
          (Cons(a, sequenceGo(xs, newR)._1), newR)
      }
    }
    rng => sequenceGo(fs, rng)
  }

  /*
  Helper function to generate a list of actions (as I'm using my custom List type from chapter3 rather than the scala List)
   */
  def generateRandomActionsList[A](n: Int)(f: Rand[A]): Part1.List[Rand[A]] = {

    @tailrec
    def generateRandomActionsListGo(n: Int, ls: Part1.List[Rand[A]])(f: Rand[A]): Part1.List[Rand[A]] = {
      if (n <= 0) ls
      else generateRandomActionsListGo(n-1, Part1.List.append(ls, Part1.List(f)))(f)
    }
    generateRandomActionsListGo(n, Part1.Nil)(f)
  }

  /*
  We can now implement ints using sequence
   */
  def intsSequence(n: Int): Rand[Part1.List[Int]] = {
    sequence(generateRandomActionsList(n)(nonNegativeInt))
  }

  /*
  The map and map2 functions allow us progress towards implementations that don't explicitly mention or pass along the RNG value. This makes it less tedious and error-prone to write
  However, there are still some functions that we can't write well using the map functions. For example the nonNegativeLessThan, which generates an integer between 0 (inclusive) and n (exclusive)

   */
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    /*
    One possible approach is to use the modulo operator to generate a non-negative integer. This does work, however the distribution will be skewed as some numbers will show up more if Int.MaxValue is not divisible by n and so it will be 'less' random than usual.
    map(nonNegativeInt) { _ % n }
     */

    /*
    We could try just recursively generate numbers if the number generated is greater than the largest multiple of n that can be fit in 32 bits, and hope we get a number that's in the
    map(nonNegativeInt) {i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) mod else nonNegativeLessThan(n)(???)

    This is close, however we don't have a rng to pass into nonNegativeInt, and it shouldn't in this case as it should return a Rand[Int] which is the function of the state transition, not just the outcome of the transition.
     */

    /*
    We could instead pass in the rng explicitly rather than using map, which would work but then that kinda goes back to the old implementation of the API. We implemented the map combinators so that would pass it automatically for us so it would be nice to have a combinator that could also achieve this.
     */
    rng =>
      val (i, rng2) = nonNegativeInt (rng)
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
  }

  def main(args: Array[String]): Unit = {
    val rng5 = new SimpleRNG(112203)
    val someRNG = new SimpleRNG(13213)
    println(double(rng5)._1)

    println(ints(9)(rng5)._1)
    println(intsSequence(9)(rng5)._1)



  }
}
