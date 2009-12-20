package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b12SpecTest extends JUnit4(b12Spec)

object b12Spec extends Specification { 
  trait TestResource extends Resource {
    override def known_methods(request: Request, response: Response) = List("GET")
    override def to_html(request: Request, response: Response) = "good stuff"
  }
  
  "webmachine" should {
    "respond with 200 when POST method is known by resource" >> {
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("good stuff")
   }
              
   "respond with '501 Not Implemented' when method is not known" >> {
      val req = new Request(httpPutRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("501")
      res.body must beEqualTo("")
   }           
 }
}