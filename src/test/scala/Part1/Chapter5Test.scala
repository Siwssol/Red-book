package Part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Chapter5Test extends AnyFlatSpec with Matchers {
  "Converting Stream to List" must "work" in {
    Part1.Stream(1,2,3,4,5,6).toList mustBe Part1.List(1,2,3,4,5,6)
    Empty.toList mustBe Part1.Nil
    Part1.Stream("ajsdfgkajshdgf").toList mustBe Part1.List("ajsdfgkajshdgf")
  }
}
