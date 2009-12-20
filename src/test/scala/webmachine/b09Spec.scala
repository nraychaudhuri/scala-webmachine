package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b09SpecTest extends JUnit4(b09Spec)

object b09Spec extends Specification { 
  trait TestResource extends Resource {
    override def malformed_request(request: Request, response: Response) = {
      request.base.getParameter("important-stuff") == null
    }
    override def to_html(request: Request, response: Response) = "good stuff"
  }
  
  "webmachine" should {
    "respond with 200 when request is well formed" >> {
      val base = httpGetRequest
      base.setParameter("important-stuff", "2345")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("good stuff")
   }
              
   "respond with '400 bad request' when request is mal-formed" >> {
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("400")
      res.body must beEqualTo("")
   }           
 }
}