package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class g07SpecTest extends JUnit4(g07Spec)

object g07Spec extends Specification { 
  trait TestResource extends Resource {
    override def charsets_provided(request: Request, response: Response) = Some(List("utf-8", "iso-8859-1"))
    override def encodings_provided(request: Request, response: Response) = {
      Some(List(
        ("reverse", (x : String) => x.reverse),
        ("identity", (x : String) => x)        
        ))
    }
    override def languages_provided(request: Request, response: Response) = Some(List("en", "es"))
    override def resource_exists(request: Request, response: Response) = true
    override def variances(request: Request, response: Response) = List("Cookie")
    override def content_types_provided(request: Request, response: Response) = {
      List(("application/json", to_json _), ("text/xml", to_xml _)) 
    }
    
    def to_json(request: Request, response: Response) = "{'name' : 'Nilanjan'}"
    def to_xml(request: Request, response: Response) = <name>Nilanjan</name>.toString
  }
  
  "webmachine" should {
    "respond with variances" >> {
        val req = new Request(httpGetRequest)
        val res = Response(new MockHttpServletResponse)
        dispatch(new Resource with TestResource, req, res)
        res.status must beEqualTo("200")
        res.headers("Vary") must beEqualTo("Accept, Accept-Charset, Accept-Encoding, Accept-Language, Cookie")
      } 
      
      "respond with '404 Not Found' when resource doesn't exists" >> {
        trait NoResource extends Resource {
          override def resource_exists(request: Request, response: Response) = false
        }
        val req = new Request(httpGetRequest)
        val res = Response(new MockHttpServletResponse)
        dispatch(new Resource with TestResource with NoResource, req, res)
        res.status must beEqualTo("404")
        res.body must beEqualTo("")
      } 
    
  }
}