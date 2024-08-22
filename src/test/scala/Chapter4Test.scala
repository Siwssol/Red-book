import org.scalatest.matchers.must.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class Chapter4Test extends AnyFlatSpec with Matchers {

  "Option map" must "work correctly" in {
    val x: Option[Int] = Some(3)
    val y: Option[String] = None
    x.map(x => x * 8) mustBe Some(24)
    y.map(_ => "kajdhfakjsf") mustBe None
  }

  "Option flatmap" must "work" in {
    val x: Option[Int] = Some(1)
    println(x.flatmap(x => None))
  }

  "Option filter" must "work" in {
    val x: Option[List[Int]] = Some(List(1))
    println(x.filter(x => x == List(9)))
  }
}
