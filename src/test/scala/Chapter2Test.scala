import Chapter2.{curry, factorial, fibonacci, isSorted, uncurry, compose}
import List.reverse
import org.scalatest.matchers.must.Matchers
import org.scalatest.flatspec.AnyFlatSpec



class Chapter2Test extends AnyFlatSpec with Matchers {

  def genList(n: Int, start: Int, step: Int)(f: Int => Int): List[Int] = {

    def genListGo(cur: Int, ls: List[Int])(f: Int => Int) : List[Int] = {
      if (cur > n)
        ls
      else genListGo(cur + step, Cons(f(cur), ls))(f)
    }
    reverse(genListGo(start, Nil)(f))
  }

  "fibonacci function" must "output correct values" in {
    fibonacci(5) mustBe 3
    fibonacci(8) mustBe 13
    fibonacci(11) mustBe 55
    List(fibonacci(1), fibonacci(2), fibonacci(3), fibonacci(4), fibonacci(5), fibonacci(6),fibonacci(7), fibonacci(8), fibonacci(9), fibonacci(10), fibonacci(11), fibonacci(12), fibonacci(13), fibonacci(14), fibonacci(15), fibonacci(16), fibonacci(17), fibonacci(18)) mustBe genList(18, 1, 1)(fibonacci)
    List(fibonacci(20), fibonacci(22), fibonacci(24), fibonacci(26), fibonacci(28)) mustBe genList(28, 20, 2)(fibonacci)
  }

  "factorial function" must "output correct values" in {
    factorial(1) mustBe 1
    factorial(2) mustBe 2
    factorial(0) mustBe 1
    List(factorial(1), factorial(2), factorial(3), factorial(4), factorial(5), factorial(6), factorial(7), factorial(8), factorial(9)) mustBe genList(9, 1, 1)(factorial)
    List(factorial(0), factorial(3), factorial(6), factorial(9), factorial(12)) mustBe genList(12, 0, 3)(factorial)
  }

  "is sorted function" must "output correct values" in {
    isSorted(Array(1,2,3,4,5), (x: Int,y: Int) => x < y) mustBe true
    isSorted(Array(1,3,2,4,5,6,3,5,324,23,6), (x: Int, y: Int) => x < y) mustBe false
  }

  "curry function" must "work" in {
    val a = curry((x: Int,y: Int) => x + y)
    a(1)(2) mustBe 3
    a(4)(5) mustBe 9
    val partialA = a(3)
    partialA(100) mustBe 103
    partialA(55) mustBe 58

    val b = curry((x: String, y: String) => x + y)
    b("aa")("bb") mustBe "aabb"
    b("This is")(" crazy!!") mustBe "This is crazy!!"
    val partialB = b("Hello")
    partialB(" World") mustBe "Hello World"
    partialB(" everyone, I am Lewis") mustBe "Hello everyone, I am Lewis"

    val c = curry((x: Int, y: Int) => genList(x, y, 1)(fibonacci))
    c(24)(20) mustBe List(4181, 6765, 10946, 17711, 28657)
  }

  "uncurry function" must "work" in {
    val a = uncurry((x: Int) => (y: Int) => x + y)
    a(1, 2) mustBe 3
    a(15, 37) mustBe 52

    val b = uncurry((x: String) => (y: String) => x + y)
    b("Hello ","World!") mustBe "Hello World!"
    b("This is an uncurried function,", " how cool is that!") mustBe "This is an uncurried function, how cool is that!"

    val c = uncurry(curry((x: Int,y: Int) => x * y))
    c(5,6) mustBe 30
    c(50, 4) mustBe 200
  }

  "compose function" must "work" in {
    def double(x: Int): Int = x * 2

    def squared(x: Int) : Int = x * x

    val quad = compose(double,double)
    quad(2) mustBe 8
    quad(6) mustBe 24
    quad(45) mustBe 180

    val fibFact = compose(fibonacci, factorial)
    fibFact(3) mustBe 5
    fibFact(4) mustBe 28657

    def repeatString(n: Int) : String = {

      def repeatHello(cur: Int, str: String) : String = {
        if (cur >= n) str
        else
          repeatHello(cur + 1, str + "hello ")
      }

      repeatHello(0, "")
    }

    val squaredHello = compose(repeatString, squared)
    squaredHello(4) mustBe "hello hello hello hello hello hello hello hello hello hello hello hello hello hello hello hello "
    squaredHello(2) mustBe "hello hello hello hello "
  }

  




}
