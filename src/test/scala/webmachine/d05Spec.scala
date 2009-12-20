package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class d05SpecTest extends JUnit4(d05Spec)

object d05Spec extends Specification { 
  var supported_langs: Option[List[String]] = _
  trait TestResource extends Resource {
    override def languages_provided(request: Request, response: Response) = {
      supported_langs
    }
    override def to_html(request: Request, response: Response) = {
      response.content_language match {
        case Some("es") => <b>hola</b>.toString
        case _ => <b>Hello</b>.toString 
      }
    }
  }
  
  "webmachine" should {
    "respond with to_html when resource don't provide languages" >> {
      supported_langs = None
      val base = httpGetRequest
      base.addHeader("accept-language", "en;q=03, es")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.content_language must beEqualTo(None)
      res.body must beEqualTo("<b>Hello</b>")
    }
     
    "respond with '406 Not Acceptable' when requested language not supported" >> {
      supported_langs = Some(List("en", "es"))
      val base = httpGetRequest
      base.addHeader("accept-language", "jp")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("406")
      res.body must beEqualTo("")
    }
        
    "respond with en content when requested language is supported" >> {
      supported_langs = Some(List("en", "es"))
      val base = httpGetRequest
      base.addHeader("accept-language", "en")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.content_language must beEqualTo(Some("en"))
      res.body must beEqualTo("<b>Hello</b>")
    }
        
    "respond with es content when requested language is supported" >> {
      supported_langs = Some(List("en", "es"))
      val base = httpGetRequest
      base.addHeader("accept-language", "es")
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.content_language must beEqualTo(Some("es"))
      res.body must beEqualTo("<b>hola</b>")
    }    
    
  }
}