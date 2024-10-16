package Part1

import Part1.Stream.consStream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Chapter5Test extends AnyFlatSpec with Matchers {
  "head option stream" must "work" in {
    Stream(1,2,3).headOption mustBe Some(1)
    Stream().headOption mustBe None
    Stream(4,2,4,4,1,3,2,3,4,1,4,3,1,3,2,3,2).headOption mustBe Some(4)
  }

  "Converting Stream to List" must "work" in {
    Stream(1,2,3,4,5,6).toList mustBe List(1,2,3,4,5,6)
    Empty.toList mustBe Nil
    Stream("ajsdfgkajshdgf").toList mustBe List("ajsdfgkajshdgf")
    Stream().toList mustBe Nil
  }

  "take n from stream" must "work" in {
    Stream(1,2,3,4,5,6,7,8,9,10).take(6).toList mustBe List(1,2,3,4,5,6)
    Stream(10).take(5).toList mustBe List(10)
    Empty.take(9).toList mustBe Nil
    Stream("a","h","k","k","d","f","r","t","t").take(3).toList mustBe List("a","h","k")
    Stream(42,24,35).take(0).toList mustBe Nil
  }

  "drop n from Stream" must "work" in {
    Stream(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18).drop(6).toList mustBe List(7,8,9,10,11,12,13,14,15,16,17,18)
    Stream(1,2,3).drop(5).toList mustBe Nil
    Empty.drop(1).toList mustBe Nil
    Stream(5,6,3,1,4).drop(0).toList mustBe List(5,6,3,1,4)
  }

  "take while from stream" must "work" in {
    Stream(1,2,3,4,5,6,7,8,9,10).takeWhile(x => x < 8).toList mustBe List(1,2,3,4,5,6,7)
    Stream(2,2,3,1,4,2,5,3,4).takeWhile(x => x == 2).toList mustBe List(2,2)
    Empty.takeWhile(x => x).toList mustBe Nil
    Stream(1,1,1,23,23,23,34,3).takeWhile(x => (x % 2) != 0).toList mustBe List(1,1,1,23,23,23)
  }

  "forall stream" must "work" in {
    Stream(1,1,1,1,1,1,1,1,1,1).forAll(x => x == 1) mustBe true
    Stream(2,24,6,2,46,28,7,0,264,680,40,6).forAll(x => (x % 2) != 0) mustBe false
    Empty.forAll(x => x) mustBe true
    Empty.forAll(x => x == "a") mustBe true
  }

  "headOption fold right" must "work" in {
    Stream(6,7,3,2,3).headOption mustBe Stream(6,4,2,5,3).headOptionRight
    Stream().headOption mustBe Empty.headOptionRight
    Stream(99,2,4,4,1,3,2,3,4,1,4,3,1,3,2,3,2).headOption mustBe Stream(99,4,3).headOptionRight
  }

  "map stream" must "work" in {
    Stream(1,2,3,4,5).map(x => x + 1).toList mustBe List(2,3,4,5,6)
    Stream("a", "b", "c", "d", "e", "f").map(x => x + x).toList mustBe List("aa", "bb", "cc", "dd", "ee", "ff")
    Empty.map(x => x).toList mustBe Nil
    Stream(1,2,3,4,5,6,7,8,9,10).map(x => if (x < 5) x + 6 else x - 3).toList mustBe List(7,8,9,10,2,3,4,5,6,7)
  }

  "filter stream" must "work" in {
    Stream(1,2,3,4,5,6,7,8,9).filter(x => (x > 3) && (x < 7)).toList mustBe List(4,5,6)
    Stream(99,2,4,4,1,3,2,3,4,1,4,3,1,3,2,3,2).filter(x => (x % 2) != 0).toList mustBe List(99,1,3,3,1,3,1,3,3)
    Stream(4,5,6,3,4,3,4,2).map(x => x * 2).filter(x => (x % 3) == 0).toList mustBe List(12, 6, 6)
  }

  "append stream" must "work" in {
    Stream(1,2,3).append(Stream(4,5,6)).toList mustBe List(1,2,3,4,5,6)
    Stream(3,1,3,2,3,1).append(Empty).toList mustBe List(3,1,3,2,3,1)
    Empty.append(Stream(999,999,3,2,3,9,9,9,9)).toList mustBe List(999,999,3,2,3,9,9,9,9)
    Empty.append(Empty).toList mustBe Nil
  }

  "flatmap stream" must "work" in {
    Stream(1,2,3,4,5).flatmap(x => Stream(x)).toList mustBe List(1,2,3,4,5)
    Stream(0,5,10).flatmap(x => Stream(x + 5, x * 5)).toList mustBe List(5,0,10,25,15,50)
    Empty.flatmap(x => x).toList mustBe Nil
    Stream(1,1,1,1).flatmap(x => Empty).toList mustBe Nil
    Stream(1,2,3).flatmap(x => Stream(x*2, x*2)).toList mustBe List(2,2,4,4,6,6)
  }

  "testing infinite streams of constants" must "work" in {
    Stream.constant(1).take(10).toList mustBe List(1,1,1,1,1,1,1,1,1,1)
    Stream.constant(6).exists(x => (x % 2) == 0) mustBe true
    Stream.constant(99).map(_ + 1).takeWhile(_ != 100).toList mustBe Nil
    Stream.constant(5).forAll(_ != 5) mustBe false
  }

  "testing infinite sequence of integers" must "work" in {
    Stream.from(30).take(5).toList mustBe List(30,31,32,33,34)
    Stream.from(1).map(x => x + 1).take(7).toList mustBe List(2,3,4,5,6,7,8)
    Stream.from(0).flatmap(x => Stream(x, x+1)).take(6).toList mustBe List(0,1,1,2,2,3)
    Stream.from(5).takeWhile(x => x < 12).toList mustBe List(5,6,7,8,9,10,11)
    Stream.from(3).exists(x => x > 100) mustBe true
  }

  "infinite sequence of fibonacci numbers" must "work" in {
    Stream.fibs.take(5).toList mustBe List(0,1,1,2,3)
    Stream.fibs.take(9).map(x => x + 1).toList mustBe Stream.fibs.map(x => x + 1).take(9).toList
    Stream.fibs.exists(x => x == 1346269) mustBe true
    Stream.fibs.take(35).forAll(x => x < 5000000) mustBe false
  }

  "stream unfold" must "work" in {
    Stream.constantFold(6).take(6).toList mustBe Stream.constant(6).take(6).toList
    Stream.fromFold(24).map(x => x - 1).take(9).toList mustBe Stream.from(23).take(9).toList
    Stream.fibsFold.take(9).toList mustBe Stream.fibs.take(9).toList
    Stream.onesFold.take(4).toList mustBe Stream.ones.take(4).toList
  }

  "map unfold" must "work" in {
    Stream(1,2,3,4,5).mapFold(x => x + 1).toList mustBe List(2,3,4,5,6)
    Stream("a", "b", "c", "d", "e", "f").mapFold(x => x + x).toList mustBe List("aa", "bb", "cc", "dd", "ee", "ff")
    Empty.mapFold(x => x).toList mustBe Nil
    Stream(1,2,3,4,5,6,7,8,9,10).mapFold(x => if (x < 5) x + 6 else x - 3).toList mustBe List(7,8,9,10,2,3,4,5,6,7)
    Stream.constant(3).take(9).mapFold(x => x + 25).toList mustBe Stream.constant(28).take(9).toList
  }

  "take unfold" must "work" in {
    Stream(1,2,3,4,5,6,7,8,9,10).takeFold(6).toList mustBe List(1,2,3,4,5,6)
    Stream(10).takeFold(5).toList mustBe List(10)
    Empty.takeFold(9).toList mustBe Nil
    Stream("a","h","k","k","d","f","r","t","t").takeFold(3).toList mustBe List("a","h","k")
    Stream(42,24,35).takeFold(0).toList mustBe Nil
  }

  "take while unfold" must "work" in {
    Stream(1,2,3,4,5,6,7,8,9,10).takeWhileFold(x => x < 8).toList mustBe List(1,2,3,4,5,6,7)
    Stream(2,2,3,1,4,2,5,3,4).takeWhileFold(x => x == 2).toList mustBe List(2,2)
    Empty.takeWhileFold(x => x).toList mustBe Nil
    Stream(1,1,1,23,23,23,34,3).takeWhileFold(x => (x % 2) != 0).toList mustBe List(1,1,1,23,23,23)
  }

  "zip with" must "work" in {
    Stream(1,2,3,4).zipWith(Stream(1,2,3,4))(_ + _).toList mustBe List(2, 4, 6, 8)
    Stream.constant(4).take(4).zipWith(Stream.fibs.take(4))((x, y) => y - x).toList mustBe List(-4, -3, -3, -2)
    Empty.zipWith(Empty: Stream[Int])((_,_)=> true).toList mustBe Nil
    Stream(1,2,3,4,5,6,7,8,9,10).zipWith(Stream(10,9,8,7,6,5,4,3,2,1))(_ * _).toList mustBe List(10, 18, 24, 28, 30, 30, 28, 24, 18, 10)
  }

  "zip all" must "work" in {
    Stream(1,2,3,4).zipAll(Stream(1,2,3,4)).toList mustBe List((Some(1),Some(1)), (Some(2),Some(2)), (Some(3),Some(3)), (Some(4),Some(4)))
    Stream.constant(9).take(9).zipAll(Stream(4,1,2,3,1)).toList mustBe List((Some(9),Some(4)), (Some(9),Some(1)), (Some(9),Some(2)), (Some(9), Some(3)), (Some(9), Some(1)), (Some(9), None), (Some(9), None), (Some(9), None), (Some(9), None))
    Stream.fibs.take(3).map(x => x + 100).zipAll(Stream(2,4,6,8,10,14,16)).toList mustBe List((Some(100),Some(2)), (Some(101), Some(4)), (Some(101), Some(6)), (None, Some(8)), (None, Some(10)), (None, Some(14)), (None, Some(16)))
    Empty.zipAll(Empty).toList mustBe Nil
  }

  "startWith" must "work" in {
    Stream(1,2,3,4).startsWith(Stream(1,2,3)) mustBe true
    Stream(6,5,3,6,7,3).startsWith(Stream(6,5,3,5)) mustBe false
    Empty.startsWith(Empty) mustBe true
    Stream(1,3,2,4,2,1).startsWith(Empty) mustBe true
    Empty.startsWith(Stream(4,2,4,3)) mustBe false
  }

  "tails" must "work" in {
    Stream(1,2,3).tails.map(x => x.toList).toList mustBe List(List(1,2,3), List(2,3), List(3))
    Stream.constant(6).take(6).tails.map(x => x.toList).toList mustBe List(List(6,6,6,6,6,6), List(6,6,6,6,6), List(6,6,6,6), List(6,6,6), List(6,6), List(6))
    Stream.fibs.take(7).tails.map(x => x.toList).toList mustBe List(List(0,1,1,2,3,5,8), List(1,1,2,3,5,8), List(1,2,3,5,8), List(2,3,5,8), List(3,5,8), List(5,8), List(8))
    Empty.tails.toList mustBe Nil
  }

  "has sub sequence" must "work" in {
    Stream(1,2,3,4,5,6,7).hasSubsequence(Stream(2,3,4)) mustBe true
    Stream(1,2,3,4,5,6,7).hasSubsequence(Stream(2,4,4)) mustBe false
    Stream(1,2,3,4,5,6,7).hasSubsequence(Stream(1,2,3,4,5,6,7)) mustBe true
    Stream(1,2,3,4,5,6,7).hasSubsequence(Empty) mustBe true
    Empty.hasSubsequence(Empty) mustBe false
    Stream(1,2,3,4,5,6,7).hasSubsequence(Stream(3,4,5,6,6)) mustBe false
    Stream("a", "b", "g", "j", "d", "f").hasSubsequence(Stream("g")) mustBe true
    Stream("a", "b", "g", "j", "d", "f").hasSubsequence(Stream("g", "d", "j")) mustBe false
  }

  "scan right" must "work" in {
    Stream(1,2,3).scanRight(0)(_ + _).toList mustBe List(6, 5, 3, 0)
    Stream(1,2,3,4,5).scanRight(Empty: Stream[Int])((x, y) => consStream(x, y)).map(x => x.toList).toList mustBe List(List(1,2,3,4,5), List(2,3,4,5), List(3,4,5), List(4,5), List(5), List())
    Stream(1,2,3,4,5,6,7,8,9).scanRight(1)((x, y) => if (x % 2 == 0) x * y else y * y).toList mustBe List(0, 0, 0, 589824, 147456, 384, 64, 8, 1, 1)
  }
}
