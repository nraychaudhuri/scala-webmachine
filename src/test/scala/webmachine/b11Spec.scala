package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b11SpecTest extends JUnit4(b11Spec)

object b11Spec extends Specification { 
  trait TestResource extends Resource {
    override def uri_too_long(request: Request, response: Response) = request.base.getRequestURI().length() > 20
    override def to_html(request: Request, response: Response) = "good stuff"
  }
  
  "webmachine" should {
    "respond with 200 when uri is not too long" >> {
      val base = httpGetRequest
      base.setRequestURI("someuri" * 2)
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("good stuff")
   }
              
   "respond with '414 Request URI Too Long' when uri is too long" >> {
      val base = httpGetRequest
      base.setRequestURI("someuri" * 10)
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("414")
      res.body must beEqualTo("")
   }           
 }
}