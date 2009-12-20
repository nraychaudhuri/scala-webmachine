package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class c04SpecTest extends JUnit4(c04Spec)

object c04Spec extends Specification { 

  trait TestResource extends Resource {
    override def content_types_provided(request: Request, response: Response) = {
      List(("application/json", to_json _), ("text/xml", to_xml _)) 
    }
    
    def to_json(request: Request, response: Response) = "{'name' : 'Nilanjan'}"
    def to_xml(request: Request, response: Response) = <name>Nilanjan</name>.toString
  }
  
  "webmachine" should {
    "respond with first content type specified when no accept provided" >> {
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("{'name' : 'Nilanjan'}")
    }
    
    "respond with content type matching the provided accept" >> {
      val base = httpGetRequest
      base.addHeader("accept", "text/xml")      
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("<name>Nilanjan</name>")
    }
               
    "respond with '406 Not Acceptable' when no content type is acceptable" >> {
      val base = httpGetRequest
      base.addHeader("accept", "image/jpeg")      
      val req = new Request(base)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("406")
      res.body must beEqualTo("")
    }           
  }
}