package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b05SpecTest extends JUnit4(b05Spec)


object b05Spec extends Specification { 
  trait TestResource extends Resource {
    override def known_content_type(request: Request, response: Response) = {
      List("text/plain", "text/html").exists { _ == request.content_type.getOrElse("").split(';')(0) }
    } 
    override def to_html(request: Request, response: Response) = "matching content type found"
  }
  
  "webmachine" should {
    "respond with 200 when matching content type found" >> {
      val httpRequest = httpGetRequest
      httpRequest.setContentType("text/html")
      val req = Request(httpRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("matching content type found")
      res.content_type must beEqualTo("text/html")
   }

   "respond with text content when text/plain content type requested" >> {
      trait PlainResource extends Resource {
        override def content_types_provided(req: Request, res: Response) = List(("text/plain", to_text _))
        def to_text(request: Request, response: Response) = "just a plain text"
      }     
     
      val httpRequest = httpGetRequest
      httpRequest.setContentType("text/plain")
      val req = Request(httpRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource with PlainResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("just a plain text")
      res.content_type must beEqualTo("text/plain")
   }

   "respond with '415 Unsupported Media Type' when no match" >> {
      val httpRequest = httpGetRequest
      httpRequest.setContentType("application/json; charset=utf-8")
      val req = Request(httpRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("415")
      res.body must beEqualTo("")
   }
      
 }
}