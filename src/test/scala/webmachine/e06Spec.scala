package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class e06SpecTest extends JUnit4(e06Spec)

object e06Spec extends Specification { 
  var supported_charsets: Option[List[String]] = _
  trait TestResource extends Resource {
    override def charsets_provided(request: Request, response: Response) = {
      supported_charsets
    }
    override def to_html(request: Request, response: Response) = {
      response.charset match {
        case Some("UTF-8") => "unicode"
        case _ => "ascii"  
      }
    }
  }
  
  "webmachine" should {
    "respond with to_html when resource don't provide supported charset" >> {
      supported_charsets = None
      val base = httpGetRequest
      base.addHeader("accept-charset", "iso-8859-1")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.charset must beEqualTo(None)
      res.body must beEqualTo("ascii")
    }
    
    "respond with '406 Not Acceptable' when requested charset not supported" >> {
      supported_charsets = Some(List("UTF-8"))
      val base = httpGetRequest
      base.addHeader("accept-charset", "latin-1")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("406")
      res.body must beEqualTo("")
    }

    "respond with UTF-8 content when resource support the charset" >> {
      supported_charsets = Some(List("UTF-8", "iso-8859-1"))
      val base = httpGetRequest
      base.addHeader("accept-charset", "UTF-8")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.charset must beEqualTo(Some("UTF-8"))
      res.body must beEqualTo("unicode")
    }
    
    "respond with 'iso-8859-1' content when resource support the charset" >> {
      supported_charsets = Some(List("UTF-8", "iso-8859-1"))
      val base = httpGetRequest
      base.addHeader("accept-charset", "iso-8859-1")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.charset must beEqualTo(Some("iso-8859-1"))
      res.body must beEqualTo("ascii")
    }
    
  }
}