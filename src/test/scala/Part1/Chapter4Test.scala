package Part1
import Part1.Chapter4._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Chapter4Test extends AnyFlatSpec with Matchers {

  "Option map" must "work correctly" in {
    val x: Option[Int] = Some(3)
    val y: Option[String] = None
    x.map(x => x * 8) mustBe Some(24)
    y.map(_ => "kajdhfakjsf") mustBe None
  }

  "Option flatmap" must "work" in {
    val x: Option[List[Int]] = Some(List(1,2,3,4))
    x.flatmap(x => Some(x)) mustBe Some(List(1,2,3,4))
    Some(List(1,2,3,4,5)).flatmap(x => Some(x, x)) mustBe Some(List(1,2,3,4,5), List(1,2,3,4,5))
  }

  "Option getOrElse" must "work" in {
    Some(46462842).getOrElse(0) mustBe 46462842
    None.getOrElse("Nothing") mustBe "Nothing"
  }

  "Option orElse" must "work" in {
    Some(1391379).orElse(Some(78)) mustBe Some(1391379)
    None.orElse(None) mustBe None
  }

  "Option filter" must "work" in {
    val x: Option[List[Int]] = Some(List(1))
    println(x.filter(x => x == List(9)))
    case class Employee(name: String, department: String)
    val emp: Option[Employee] = Some(Employee("Joe", "Accounting"))
    emp.map(_.department).filter(_ == "Accounting").getOrElse("Default department") mustBe "Accounting"
  }

  "Option map2" must "work" in {
    map2(Some(56), Some(67))(_ + _) mustBe Some(123)
    map2(None: Option[Int], Some(2))(_ + _) mustBe None
    map2(Some("alksdjhfasl"), None: Option[String])(_ + _) mustBe None
    map2(Some(List(1,4,2,5,6,3)), Some(List(6,2,5,3,4,3)))((x, y) => {
      val newX = List.map(x)(a => if (a%2 == 0) a*3 else a + 1)
      val newY = List.map(y)(a => a * 8)
      List.zipWith(newX, newY)((a, b) => a*b)
    }) mustBe Some(List(96, 192, 240, 144, 576, 96))
  }

  "Option sequence" must "work" in {
    sequence(List(Some(12), Some(65), Some(1), Some(10), Some(423), Some(31))) mustBe Some(List(12,65,1,10,423,31))
    sequence(List(Some("A"), Some("B"), Some("C"), None, Some("D"), Some("E"), Some("F"), Some("G"))) mustBe None
    sequence(List.map(List(1,1,1,1,1,2,1,1,1,1,1,1))(x => if (x%2 != 0) Some(x) else None)) mustBe None
  }

  "Option traverse" must "work" in {
    traverse(List(1,2,3,4,5,6,7,8,9))(x => Some(x)) mustBe Some(List(1,2,3,4,5,6,7,8,9))
    traverse(List("1", "5", "4", "7", "43", "423", "57389", "263784"))(x => Try(x.toInt)) mustBe Some(List(1,5,4,7,43,423,57389, 263784))
  }

  "Either map" must "work" in {
    Right(6).map(x => x+1) mustBe Right(7)
    Left(404).map(x => x) mustBe Left(404)
  }

  "Either traverse" must "work" in {
    traverseEither(List(1,3,5,1,2,3,4,5))(a => if (a%2 == 0) Left(a) else Right(a)) mustBe Left(2)
    traverseEither(List(11,35,53,41,675,53,49,5))(a => if (a%2 == 0) Left(a) else Right(a+20)) mustBe Right(List(31,55,73,61,695,73,69,25))
  }
}
