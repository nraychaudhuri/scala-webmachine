package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b06SpecTest extends JUnit4(b06Spec)


object b06Spec extends Specification { 
  
  "webmachine" should {
    "respond with 200 when valid content headers" >> {
      trait TestResource extends Resource {
        override def valid_content_headers(request: Request, response: Response) = {
          true
        }
        override def to_html(request: Request, response: Response) = "some body"
      }
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("some body")
   }
        
   "respond with '501 Not implemented' when invalid content headers" >> {
      trait TestResource extends Resource {
        override def valid_content_headers(request: Request, response: Response) = {
          false
        }
      }
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("501")
      res.body must beEqualTo("")
   }     


 }
}