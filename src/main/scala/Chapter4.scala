

trait Option[+A] {
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
    val x: Option[Int] = Some(3)
    val abs7 = lift(math.abs)
    println(variance(Seq(3,4,5,2)))
    println(parseInsuranceRateQuote("0", "7"))
    println(sequence(List(Some(1), Some(2), Some(5), Some(7), Some(9))))
    println(sequenceTraverse(List(Some(1), Some(2), Some(5), Some(7), Some(9))))

  }

}


