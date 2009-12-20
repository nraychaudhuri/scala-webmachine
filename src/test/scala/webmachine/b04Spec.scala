package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b04SpecTest extends JUnit4(b04Spec)


object b04Spec extends Specification { 
  trait TestResource extends Resource {
    override def valid_entity_length(request: Request, response: Response) = {
      request.content_length < 30
    } 
    override def to_html(request: Request, response: Response) = "some body"
  }
  
  "webmachine" should {
    "respond with 200 when entity length is valid" >> {
      val httpRequest = httpGetRequest
      httpRequest.setContent("nilanjan".getBytes)
      val req = Request(httpRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("some body")
   }   

    "set response status to '413 Request Entity Too Large' when entity length crosses limit" >> {
      val httpRequest = httpGetRequest
      httpRequest.setContent("This is a really big entity and should fail the length check".getBytes)
      val req = Request(httpRequest)
      val res = Response(new MockHttpServletResponse)
      Dispatcher.dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("413")
      res.body must beEqualTo("")
   }   

 }
}