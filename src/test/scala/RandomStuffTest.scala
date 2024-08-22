import Chapter2.genList
import RandomScalaPractice.collatzSteps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RandomStuffTest extends AnyFlatSpec with Matchers{

  "Collatz steps" must "correctly output the right number of steps for a number to 'potentially' reach 1 (as we don't know if every number will do that)" in {
    collatzSteps(1) mustBe 0
    collatzSteps(9780657630L) mustBe 1132
    collatzSteps(989345275647L) mustBe 1348
  }
}
