package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b10SpecTest extends JUnit4(b10Spec)

object b10Spec extends Specification { 
  trait TestResource extends Resource {
    override def allowed_methods(request: Request, response: Response) = List("GET")
    override def to_html(request: Request, response: Response) = "good stuff"
  }
  
  "webmachine" should {
    "respond with 200 when GET request is allowed" >> {
      val base = httpGetRequest
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("good stuff")
   }
              
   "respond with '405 Method Not Allowed' when POST is not allowed" >> {
      val req = new Request(httpPostRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("405")
      res.body must beEqualTo("")
   }           
 }
}