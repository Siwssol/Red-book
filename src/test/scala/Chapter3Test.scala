import List.sum
import org.mockito.MockitoSugar.{mock, when}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Chapter3Test extends AnyFlatSpec with Matchers {

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
