
/*
 Options are useful with error handling as it can tell you whether you have an error (in that case you can just return None). Then you can use that None to stop other functions from executing.
 However, because they only return None, they do not tell you what the error actually was, which typically is something that would be useful to us. They only tell you if an error occurred.
 To encapsulate a reason for the error we can use the Either data type.
 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(get) => Some(f(get))
      case None => None
    }
  }

  def flatmap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(get) => f(get)
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B) : B = {
    this match {
      case Some(get) => get
      case None => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (this != None) this
    else ob
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(get) =>
        if (f(get)) Some(get)
        else None
      case None => None
    }
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

/*
Eithers allow us to encapsulate the kind of error that occured. It takes two cases, like Option, but both of those cases carry a value. It represents values that can be one of two things,
a disjoint union of the two types.
The two cases are Left and Right:
  Left -> Usually represents the failure/error case
  Right -> Typically represents the success case (as "right" means correct)

 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(value) =>
        try Right(f(value))
        catch { case e: E => Left(e) }
      case Left(e) => Left(e)
    }
  }

  def flatmap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(value) =>
        try f(value)
        catch { case e: EE => Left(e) }
      case Left(e) => Left(e)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(_) => this
      case Left(_) => b
    }
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

case class Employee(name: String, department: String, manager: Option[String] = None) {

}

object Employee {

  def lookupByName(name: String): Option[Employee] = {
    if (name == "Joe") Some(Employee(name, "fs", Some("Steve")))
    else None
  }

  def main(args: Array[String]) : Unit = {
    val joeDepartment: String = lookupByName("joe").map(_.department).getOrElse("Default department")
    val joeManager: Option[String] = lookupByName("Joe").flatmap(_.manager)
    println(joeDepartment)
    println(joeManager)
  }

}

object Chapter4 {

  /*
  Partial function:
     means it's not defined for some inputs

  We can make mean a total function by making it return a value of type Option. That way for every input, we have a determined output
   */
  def mean(ls: Seq[Double]): Option[Double] = {
    if (ls.isEmpty) None
    else Some(ls.sum / ls.length)
  }

  /*
  Here now instead of just returning None, we can return a String in case of failure
   */
  def meanEither(ls: Seq[Double]): Either[String, Double] = {
    if (ls.isEmpty)
      Left("mean of empty List!")
    else
      Right(ls.sum/ls.length)
  }

  /*
  We can return an exception on the left side which encapsulates the info about the error as a value
   */
  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch { case e: Exception => Left(e) }
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    val meanOfxs: Option[Double] = mean(xs)
    val xsOption: Option[Seq[Double]] = {
      if (xs.isEmpty) None
      else Some(xs)
    }

    def f(ls: Seq[Double]) : Option[Double] = {
      meanOfxs match {
        case Some(getMean) => mean(ls.map(x => math.pow(x - getMean, 2)))
      }
    }

    xsOption.flatmap(f)
  }

  /*
  Turns a function of type A => B into a function of type Option[A] => Option[B].
  _ will be some function that's an option of A =
  map f will give you an option of B
  then _ map f will be a curried function of Option[A] => Option[B]
   */
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    if (a != None && b != None) {
      (a, b) match {
        case (Some(getA), Some(getB)) => Some(f(getA,getB))
      }
    }
    else None
  }

  /*
  This is the 'answer' in the book.
    It essentially maps the Option[A] into an Option[C] which makes sense as the function f in its curried form: Option[A] => Option[B] => Option[C]
    So it maps the A in Option[A] into a map of the b in Option[B] into C. As we have the A and B we can apply f to get C. However map returns an Option so we are left with Option[C]
    This means A gets mapped to Option[C] which results in Option[Option[C]] which is why we call flatmap on A to reduce it back to a single Option.

  In general for n parameters, we would call flatmap for each parameter to reduce the result to a single Option. For the last parameter we call map as it will return an option of the mapped result.
   */
  def map2Improved[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatmap (aa => b map (bb => f(aa,bb)))
  }

  /*
  Uses for-comprehension instead

  def map2ForComp[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
  }

   */

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case Cons(h, t) => h match {
        case None => None
        case Some(getHead) => sequence(t) match {
          case None => None
          case Some(getList) => Some(Cons(getHead, getList))
        }
      }
    }
  }

  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] ={
    traverse(a)(x => x)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case Cons(h, t) => f(h) match {
        case None => None
        case Some(getHead) => traverse(t)(f) match {
          case None => None
          case Some(getTail) => Some(Cons(getHead, getTail))
        }
      }
    }
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }

  def TryEither[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }

  def sequenceEither[E, A](es: List[Either[E,A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil)
      case Cons(x, xs) => x match {
        case Left(value) => Left(value)
        case Right(getHead) => sequenceEither(xs) match {
          case Left(value) => Left(value)
          case Right(getTail) => Right(Cons(getHead, getTail))
        }
      }
    }
  }

  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case Cons(x, xs) => f(x) match {
        case Left(e) => Left(e)
        case Right(getHead) => traverseEither(xs)(f) match {
          case Left(e) => Left(e)
          case Right(getTail) => Right(Cons(getHead, getTail))
        }
      }
    }
  }

  def insuranceRateQuote(value: Int, value1: Int): Double = {
    (value * 3) + (value1 * 7) - 5
  }

  def parseInsuranceRateQuote(
                             age: String,
                             numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def main(args: Array[String]) : Unit = {
    val x: List[Int] = List(1,4,5,1,2,3,4,5)
    println(traverseEither(x)(a => if (a%2 == 0) Left(a) else Right(a)))
  }

}


