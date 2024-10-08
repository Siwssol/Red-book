package Part1

import Chapter2.{curry, factorial, fibonacci, genList}
import Part1.List._
import org.mockito.MockitoSugar.{mock, when}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Chapter3Test extends AnyFlatSpec with Matchers {
  "tail function" must "correctly remove the first element in a list" in {
    tail(List(1, 2, 3, 4, 5)) mustBe List(2, 3, 4, 5)
    tail(Nil) mustBe Nil
    tail(List(1)) mustBe Nil
    tail(List("a", "b", "c", "d", "e", "f", "g")) mustBe List("b", "c", "d", "e", "f", "g")
    tail(List(List(2), List(3), List(4), List(1), List(5))) mustBe List(List(3), List(4), List(1), List(5))
  }

  "set head function" must "correctly replace the first element with the new element" in {
    setHead(2, List(1, 2, 3, 4, 5)) mustBe List(2, 2, 3, 4, 5)
    setHead(1, Nil) mustBe List(1)
    setHead("a", List("b", "b", "c", "d", "e")) mustBe List("a", "b", "c", "d", "e")
    setHead(List(List(1)), List(List(List(2), List(3)), List(List(5)), List(List(3), List(4), List(7)))) mustBe List(List(List(1)), List(List(5)), List(List(3), List(4), List(7)))
  }

  "drop function" must "correctly remove the first n elements" in {
    drop(List(1,2,3,4,5), 2) mustBe List(3,4,5)
    drop(List(1,1,1), 5) mustBe Nil
    drop(Nil, 2) mustBe Nil
    drop(List(List("a", "b"), List("g", "afsd", "j"), List("fasdf"), List("h", "h", "h"), List("123" , "345")), 2) mustBe List(List("fasdf"), List("h", "h", "h"), List("123" , "345"))
  }

  "drop while function" must "correctly remove elements based on a predicate" in {
    dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3) mustBe List(3, 4, 5)
    dropWhile(List(1, 2, 3, 4, 5, 6, 7, 8, 9), (x: Int) => x != 8) mustBe List(8, 9)
    dropWhile(List(1,1,1,1,1,1,1,1,1,1,1,1,1), (_: Int) => true) mustBe Nil
    dropWhile(List(1, 2, 3, 4, 5), (_: Int) => false) mustBe List(1,2,3,4,5)
    dropWhile(List(3,5,7,11,17,9,3,8,5,2,6,4,5,3,6,1), (x: Int) => x % 2 != 0) mustBe List(8,5,2,6,4,5,3,6,1)
  }

  "init function" must "correctly remove the last element of a list" in {
    init(List(1,2,3,4,5)) mustBe List(1,2,3,4)
    init(List(List(1), List(4), List(5))) mustBe List(List(1), List(4))
    init(Nil) mustBe Nil
    init(List(1)) mustBe Nil
  }

  "drop while improved" must "work like drop while but we don't need to specify the type of the parameter in the anonymous function" in {
    val dropperImp = dropWhileImproved(List(1,2,3,4))(x => x != 1)
    dropperImp mustBe List(1,2,3,4)
    val x: List[Int] = List(1,2,3,4)
    val dropperCur = curry((x: List[Int], y: Int => Boolean ) => dropWhile(x, y))
    dropperImp == dropperCur(List(1,2,3,4))(x => x != 1) mustBe true

    dropWhileImproved(List(2,4,62,64,64,8,6,11,6,5,80,6,7,42,0,9))(x => x % 2 == 0) mustBe List(11,6,5,80,6,7,42,0,9)
  }

  "fold right" must "work correctly" in {
    foldRight(List(1,2,3,4,5), 0)((x, y) => x + y) mustBe 15
    foldRight(List(1,1,1,1,1,1), Nil: List[Int])((x, y) => Cons(x+1, y)) mustBe List(2, 2, 2, 2, 2, 2)
    foldRight(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 1)((x, y) => x * y) mustBe factorial(9)
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) mustBe List(1,2,3)
    foldRight(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"), "")((x, y) => x + y) mustBe "abcdefghijklmn"
  }

  "foldLeft" must "work correctly" in {
    foldLeft(List(1,2,3,4,5), 0)((x, y) => x + y) mustBe 15
  }

  "foldRightViafoldLeft" must "work correctly" in {
    foldRightViaFoldLeft(List(1,2,3,4,5), 0)(_ + _) mustBe 15
  }

  "foldLeftViafoldRight" must "work correctly" in {
    foldLeftViaFoldRight(List(1,2,3,4,5), 0)(_ + _) mustBe 15
  }

  "appendleft" must "work correctlt" in {
    appendLeft(List(1,2,3))(List(4,5,6)) mustBe List(1,2,3,4,5,6)
    appendLeft(List(1,1,1,1,1))(Nil) mustBe List(1,1,1,1,1)
    appendLeft(List("h", "e", "l", "l", "o"))(List("w", "o", "r", "l", "d", "!")) mustBe List("h", "e", "l", "l", "o", "w", "o", "r", "l", "d", "!")
    appendLeft(Nil)(List(7,3,45,5,6,3)) mustBe List(7,3,45,5,6,3)
    appendLeft(Nil)(Nil) mustBe Nil
  }

  "length" must "work correctly" in {
    List.length(List(1,1,1,1,1,1,1)) mustBe 7
    List.length(Nil) mustBe 0
    List.length(List(List(List(List(5))))) mustBe 1
    List.length(List(List("f"), List("h", "g", " asdfa ", " kafksdlf"), List("fasdfad", "asdfasdf", "asdfa"), List("h"))) mustBe 4
  }

  "reverse" must "work correctly" in {
    reverse(Nil) mustBe Nil
    reverse(List(1,2,3,4,5)) mustBe List(5,4,3,2,1)
    reverse(genList(9, 1, 1)(fibonacci)) mustBe List(21, 13, 8, 5, 3, 2, 1, 1, 0)
    reverse(List("h", "e", "l", "l", "o", "w", "o", "r", "l", "d", "!")) mustBe List("!", "d", "l", "r", "o", "w", "o", "l", "l", "e", "h")
  }

  "append" must "work correctly" in {
    append(List(1,2,3), List(4,5,6)) mustBe List(1,2,3,4,5,6)
    append(List(1,1,1,1,1), Nil) mustBe List(1,1,1,1,1)
    append(List("h", "e", "l", "l", "o"), List("w", "o", "r", "l", "d", "!")) mustBe List("h", "e", "l", "l", "o", "w", "o", "r", "l", "d", "!")
    append(Nil, List(7,3,45,5,6,3)) mustBe List(7,3,45,5,6,3)
    append(Nil, Nil) mustBe Nil
  }

  "concatenate" must "work correctly" in {
    concatenate(List(List(1,2,3), List(4,5,6))) mustBe List(1,2,3,4,5,6)
    concatenate(List(List("h", "e", "l", "l", "o"), List("w", "o", "r", "l", "d", "!"), List("im"), List("L","e","w","i","s"))) mustBe List("h", "e", "l", "l", "o","w", "o", "r", "l", "d", "!", "im", "L","e","w","i","s")
    concatenate(List(Nil)) mustBe List()
    concatenate(List(List(1,3,5,7,9), List(2,4,6,8,0))) mustBe List(1,3,5,7,9,2,4,6,8,0)
    concatenate(List(List(List(1,1), List(1,1)), List(List(2,2), List(2,2), List(2,2)))) mustBe List(List(1,1), List(1,1), List(2,2), List(2,2), List(2,2))

  }

  "add one to each value" must "work correctly" in {
    addOneToEachValue(List(1,1,1,1,1,1)) mustBe List(2,2,2,2,2,2)
    addOneToEachValue(List(3,4,5,6,7,8,9,10)) mustBe List(4,5,6,7,8,9,10,11)
    addOneToEachValue(Nil) mustBe Nil
  }

  "Double To String" must "work correctly" in {
    eachDoubleToString(List(1,2,3,4,5,6,7)) mustBe List("1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0")
    eachDoubleToString(List(1.1, 2.1, 3.1, 4.1)) mustBe List("1.1", "2.1", "3.1", "4.1")
    eachDoubleToString(Nil) mustBe Nil
  }

  "map" must "work correctly" in {
    map(List(1,4,1,5,2,6,4))(x => x * 4) mustBe List(4,16,4,20,8,24,16)
    map(List("g", "h", "b", "asdfas", "g", "g", "l", "k"))(x => if (x=="g") 1 else 0) mustBe List(1,0,0,0,1,1,0,0)
    map(List(1,2,3,4,5,6,7,8,9))(x => if (x < 4) x * x else x * x * x) mustBe List(1,4,9,64, 125, 216, 343, 512, 729)
  }

  "filter" must "work correctly" in {
    filter(List(3, 4, 5, 6, 7, 4, 4, 3, 5, 4, 6, 7))(x => x!=4) mustBe List(3,5,6,7,3,5,6,7)
    filter(genList(12, 1, 1)(fibonacci))(x => (x % 2) == 0) mustBe List(0, 2, 8, 34)
    filter(genList(19, 0, 1)(x => math.pow(2, x).toInt))(x => x >= 200) mustBe List(256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288)
  }

  "flatmap" must "work correctly" in {
    flatmap(List(List(1),List(2),List(3)))(x => List(x,x)) mustBe List(List(1), List(1), List(2), List(2), List(3), List(3))
    flatmap(List(1,2,3))(x => List(x,x)) mustBe List(1,1,2,2,3,3)
    flatmap(List(1,2,3,4,5))(_ => List("a")) mustBe List("a", "a", "a", "a", "a")
    flatmap(flatmap(List(List(1),List(2),List(3)))(x => List(x,x)))(x => x) mustBe List(1,1,2,2,3,3)
    flatmap(List(List(List(1)), List(List(2)), List(List(3))))(x => List(x, x)) mustBe List(List(List(1)), List(List(1)), List(List(2)), List(List(2)), List(List(3)), List(List(3)))
  }

  "flatmap filter" must "work correctly" in {
    filterflatmap(List(1,2,3,4,5,6,7,8,9))(x => (x % 2) == 0) mustBe List(2,4,6,8)
    filterflatmap(genList(12, 1, 1)(fibonacci))(x => (x % 2) == 0) mustBe List(0, 2, 8, 34)
    filterflatmap(genList(19, 0, 1)(x => math.pow(2, x).toInt))(x => x >= 200) mustBe List(256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288)
  }

  "add lists" must "work correctly" in {
    addLists(List(1,2,3), List(4,5,6)) mustBe List(5,7,9)
    addLists(List(1,1,1,1,1,1), List(1,1,1)) mustBe List(2,2,2)
    addLists(List(4,5,3), List(2,6,3,5,4,6,3)) mustBe List(6,11,6)
    addLists(Nil, List(7,3,2)) mustBe Nil
    addLists(List(0,0,0), Nil) mustBe Nil
  }

  "zipWith" must "work correctly" in {
    zipWith(List(1,2,3), List(4,5,6))((x, y) => x + y) mustBe List(5,7,9)
    zipWith(List(6,2,4,6), List(2, 4, 7, 8, 9, 3, 3))((x, y) => x * y) mustBe List(12, 8, 28, 48)
    zipWith(List("a", "b", "c"), List("a", "b", "c"))((x, y) => x + y) mustBe List("aa", "bb", "cc")
    zipWith(Nil, List(3.0, 4.5, 7.7, 7.8))((x, y) => x.max(y)) mustBe Nil
    zipWith(List(1,5,52,3,47,32,6,54,5,3,2,6), List(7,1,50, 4, 48, 16, 4, 55, 4, 4,9, 0))((x, y) => x.max(y)) mustBe List(7,5,52,4,48,32,6,55,5,4,9,6)
  }

  "has subsequence" must "work correctly" in {
    hasSubsequence(List(1,2,3,4,5,6,7), List(2,3,4)) mustBe true
    hasSubsequence(List(1,2,3,4,5,6,7), List(2,4,4)) mustBe false
    hasSubsequence(List(1,2,3,4,5,6,7), List(1,2,3,4,5,6,7)) mustBe true
    hasSubsequence(List(1,2,3,4,5,6,7), Nil) mustBe true
    hasSubsequence(Nil, Nil) mustBe true
    hasSubsequence(List(1,2,3,4,5,6,7), List(3,4,5,6,6)) mustBe false
    hasSubsequence(List("a", "b", "g", "j", "d", "f"), List("g")) mustBe true
    hasSubsequence(List("a", "b", "g", "j", "d", "f"), List("g", "d", "j")) mustBe false
  }

  "tree size" must "work correctly" in {
    Tree.size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) mustBe 7
    Tree.size(Branch(Branch(Leaf("a"), null), Branch(Leaf("c"), Leaf("d")))) mustBe 6
    Tree.size(Branch(Branch(Branch(Branch(Leaf("gas"), Leaf("water")), Branch(null, Branch(null, Leaf("hot")))), Branch(Branch(Branch(Leaf("asd"), null), null), Branch(null, Leaf("jk")))), Branch(Branch(Branch(Branch(Branch(Leaf("asdfljk"), null), null), Leaf("fjsjf")), Branch(Leaf("asdf"), null)), Leaf("d")))) mustBe 25
    Tree.size(null) mustBe 0
  }

  "tree maximum" must "work correctly" in {
    Tree.maximum(Branch(Branch(Leaf(4), Leaf(9)), Branch(Leaf(-1), Leaf(6)))) mustBe 9
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))) mustBe 1
    Tree.maximum(null) mustBe Int.MinValue
    Tree.maximum(Branch(Branch(Leaf(10000), null), Branch(Leaf(12376), Leaf(4623)))) mustBe 12376
  }

  "Tree depth" must "work correctly" in {
    Tree.depth(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) mustBe 2
    Tree.depth(Branch(Branch(Branch(Branch(Leaf("a"), Leaf("b")), Leaf("b")), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) mustBe 4
    Tree.depth(Branch(Leaf("a"), Leaf("b")))
    Tree.depth(Branch(Branch(Leaf("a"), Branch(Branch(Leaf("a"), Branch(Leaf("a"), Branch(Leaf("a"), Branch(Leaf("a"), Leaf("b"))))), Leaf("b"))), Branch(Branch(Leaf("a"), Branch(Leaf("a"), Leaf("b"))), Leaf("d")))) mustBe 7
  }

  "Tree map" must "work correctly" in {
    Tree.map(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))))(x => x + "hello") mustBe Branch(Branch(Leaf("ahello"), Leaf("bhello")), Branch(Leaf("chello"), Leaf("dhello")))
    Tree.map(Branch(Branch(Branch(Branch(Leaf(15), Leaf(0)), Leaf(1)), Leaf(3)), Branch(Leaf(6), Leaf(7))))(x => x * 2) mustBe Branch(Branch(Branch(Branch(Leaf(30), Leaf(0)), Leaf(2)), Leaf(6)), Branch(Leaf(12), Leaf(14)))
    Tree.map(Branch(Leaf("a"), Leaf("b")))(x => x) mustBe Branch(Leaf("a"), Leaf("b"))
    Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(x => x + 1) mustBe Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5)))

  }

  "Tree functions implemented with fold" must "work correctly" in {
    Tree.size2(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) mustBe 7
    Tree.size2(Branch(Branch(Leaf("a"),  Leaf("b")), Branch(Leaf("c"), Leaf("d")))) mustBe 7
    Tree.size2(Branch(Branch(Branch(Branch(Leaf("gas"), Leaf("water")), Branch( Leaf("b"), Branch( Leaf("b"), Leaf("hot")))), Branch(Branch(Branch(Leaf("asd"), Leaf("b")), Leaf("b")), Branch(Leaf("b"), Leaf("jk")))), Branch(Branch(Branch(Branch(Branch(Leaf("asdfljk"), Leaf("b")), Leaf("b")), Leaf("fjsjf")), Branch(Leaf("asdf"), Leaf("b"))), Leaf("d")))) mustBe 33
    Tree.size2(Leaf("b")) mustBe 1

    Tree.maximum2(Branch(Branch(Leaf(4), Leaf(9)), Branch(Leaf(-1), Leaf(6)))) mustBe 9
    Tree.maximum2(Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))) mustBe 1
    Tree.maximum2(Leaf(0)) mustBe 0
    Tree.maximum2(Branch(Branch(Leaf(10000), Leaf(8423)), Branch(Leaf(12376), Leaf(4623)))) mustBe 12376

    Tree.depth2(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) mustBe 2
    Tree.depth2(Branch(Branch(Branch(Branch(Leaf("a"), Leaf("b")), Leaf("b")), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) mustBe 4
    Tree.depth2(Branch(Leaf("a"), Leaf("b")))
    Tree.depth2(Branch(Branch(Leaf("a"), Branch(Branch(Leaf("a"), Branch(Leaf("a"), Branch(Leaf("a"), Branch(Leaf("a"), Leaf("b"))))), Leaf("b"))), Branch(Branch(Leaf("a"), Branch(Leaf("a"), Leaf("b"))), Leaf("d")))) mustBe 7

    Tree.map2(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))))(x => x + "hello") mustBe Branch(Branch(Leaf("ahello"), Leaf("bhello")), Branch(Leaf("chello"), Leaf("dhello")))
    Tree.map2(Branch(Branch(Branch(Branch(Leaf(15), Leaf(0)), Leaf(1)), Leaf(3)), Branch(Leaf(6), Leaf(7))))(x => x * 2) mustBe Branch(Branch(Branch(Branch(Leaf(30), Leaf(0)), Leaf(2)), Leaf(6)), Branch(Leaf(12), Leaf(14)))
    Tree.map2(Branch(Leaf("a"), Leaf("b")))(x => x) mustBe Branch(Leaf("a"), Leaf("b"))
    Tree.map2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(x => x + 1) mustBe Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5)))

  }

}

class MockingTest extends AnyFlatSpec with Matchers {

  // Example of Mocking
  case class WorkService() {
    def getDocument(id: String): String = throw new Exception("Can't connect")

    def patchDocument(id: String, classificaton: String): String = throw new Exception("Can't connect")
  }

  case class RAVNMLService() {
    def classify(document: String): String = throw new Exception("Can't connect")
  }

  def enrichDocument(id: String, workService: WorkService, ravnmlService: RAVNMLService): String = {
    val document = workService.getDocument(id)
    val classification = ravnmlService.classify(document)
    val response = workService.patchDocument(id, classification)
    if (response == "200")
      s"Document classified and patched successfully. [docId: $id, response: $response]"
    else
      s"Something went wrong. [docId: $id, response: $response]"
  }



  // above is actual code (pretend ok? ty!)

  /*
Creates a mocked version of the services, kinda like a virtual version. on it's own, they won't do anything so calling the functions will give you an error.
However you can use the mocked services to return values if the functions were actually called, so you can test different sets of values.
 */
  private val mockedWorkedService = mock[WorkService]
  private val mockedRavnService = mock[RAVNMLService]

  "Success case" must "work" in {

    /*
  Here you set the mocked services to return certain values given a particular input.
  For example you "pretend" that WorkService returns "Document1" when you give it id "aaa"
  Then you can pinpoint where in the code it fails
   */
    when(mockedWorkedService.getDocument("aaa")).thenReturn("Document1")
    when(mockedRavnService.classify("Document1")).thenReturn("Agreement")
    when(mockedWorkedService.patchDocument("aaa", "Agreement")).thenReturn("200")
    enrichDocument("aaa", mockedWorkedService, mockedRavnService) mustBe (
      s"Document classified and patched successfully. [docId: aaa, response: 200]"
      )

  }

  "Failure case " must "work" in {
    when(mockedWorkedService.getDocument("bbb")).thenReturn("Document2")
    when(mockedRavnService.classify("Document2")).thenReturn("Agreement")
    when(mockedWorkedService.patchDocument("bbb", "Agreement")).thenReturn("404")
    enrichDocument("bbb", mockedWorkedService, mockedRavnService) mustBe (
      s"Something went wrong. [docId: bbb, response: 404]"
      )

  }
}
