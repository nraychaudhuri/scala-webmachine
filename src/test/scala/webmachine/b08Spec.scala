package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b08SpecTest extends JUnit4(b08Spec)

object b08Spec extends Specification { 
  
  "webmachine" should {
    "respond with 200 when resource is authorized" >> {
      trait TestResource extends Resource {
        override def is_authorized(request: Request, response: Response) = true
        override def to_html(request: Request, response: Response) = "some body"
      }
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("some body")
   }
           
   "respond with '401 unauthorized' when resource is forbidden" >> {
      trait TestResource extends Resource {
        override def is_authorized(request: Request, response: Response) = {
          request.hasHeader("authorizaton-key")
        }
      }
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("401")
      res.body must beEqualTo("")
   }        
 }
}