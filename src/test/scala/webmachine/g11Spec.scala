package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class g11SpecTest extends JUnit4(g11Spec)

object g11Spec extends Specification { 
  trait TestResource extends Resource {
    override def resource_exists(request: Request, response: Response) = true
    override def generate_etag(request: Request, response: Response) = Some("bar")
    override def to_html(request: Request, response: Response) = "foo"
  }
  
  "webmachine" should {
    "respond with 200 when no if-match found in request header" >> {
        val req = new Request(httpGetRequest)
        val res = Response(new MockHttpServletResponse)
        dispatch(new Resource with TestResource, req, res)
        res.status must beEqualTo("200")
        res.body must beEqualTo("foo")
     } 
    "respond with 200 when if-match found in request header and it matches" >> {
        val base = httpGetRequest
        base.addHeader("if-match", "bar")
        val req = new Request(httpGetRequest)
        val res = Response(new MockHttpServletResponse)
        dispatch(new Resource with TestResource, req, res)
        res.status must beEqualTo("200")
        res.body must beEqualTo("foo")
      }
       
    "respond with '412 Precondition Failed' when if-match found in request header but doesn't match" >> {
        val base = httpGetRequest
        base.addHeader("if-match", "baz")
        val req = new Request(base)
        val res = Response(new MockHttpServletResponse)
        dispatch(new Resource with TestResource, req, res)
        res.status must beEqualTo("412")
        res.body must beEqualTo("")
      } 
      
  }
}