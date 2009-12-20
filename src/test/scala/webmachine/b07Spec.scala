package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b07SpecTest extends JUnit4(b07Spec)


object b07Spec extends Specification { 
  
  "webmachine" should {
    "respond with 200 when resource is not forbidden" >> {
      trait TestResource extends Resource {
        override def forbidden(request: Request, response: Response) = false
        override def to_html(request: Request, response: Response) = "some body"
      }
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("some body")
   }
           
   "respond with '403 forbidden' when resource is forbidden" >> {
      trait TestResource extends Resource {
        override def forbidden(request: Request, response: Response) = true
      }
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("403")
      res.body must beEqualTo("")
   }        
 }
}