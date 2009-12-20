package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class f07SpecTest extends JUnit4(f07Spec)

object f07Spec extends Specification { 
  var supported_encodings:Option[List[(String, (String) => String)]] = _
  trait TestResource extends Resource {
    override def encodings_provided(request: Request, response: Response) = {
      supported_encodings
    }
    override def to_html(request: Request, response: Response) = "repl"
  }
  
  "webmachine" should {
    "respond with to_html when resource don't provide supported encodings" >> {
      supported_encodings = None
      val base = httpGetRequest
      base.addHeader("accept-encoding", "reverse")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.content_encoding must beEqualTo(None)
      res.body must beEqualTo("repl")
    } 
    
    "respond with '406 Not Acceptable' when requested encoding not supported" >> {
      supported_encodings = Some(List(("reverse", (x : String) => x.reverse)))
      val base = httpGetRequest
      base.addHeader("accept-encoding", "gzip")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("406")
      res.body must beEqualTo("")
    }

    "respond with reverse encoded content when requested reverse encoding is supported" >> {
      supported_encodings = Some(List(("reverse", (x : String) => x.reverse)))
      val base = httpGetRequest
      base.addHeader("accept-encoding", "reverse")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.content_encoding must beEqualTo(Some("reverse"))
      res.body must beEqualTo("lper")
    }
    
    "respond with identity content when requested identity encoding is supported" >> {
      supported_encodings = Some(List(
        ("reverse", (x : String) => x.reverse),
        ("identity", (x : String) => x)        
        ))
      val base = httpGetRequest
      base.addHeader("accept-encoding", "identity")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.content_encoding must beEqualTo(Some("identity"))
      res.body must beEqualTo("repl")
    }
       
  }
}