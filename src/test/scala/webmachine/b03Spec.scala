package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b03SpecTest extends JUnit4(b03Spec)


object b03Spec extends Specification { 
  trait TestResource extends Resource {
    override def allowed_methods(request: Request, response: Response) = List("GET", "OPTIONS")  
    override def options(request: Request, response: Response) = List(("X-mas", "happy holiday"))
    override def to_html(request: Request, response: Response) = <b>hello world</b>.toString
  }
  
  "webmachine" should {
    "respond with 200 when GET method is used" >> {
      val req = Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("<b>hello world</b>")
      res.headers must notHaveKey("X-mas")
   }
   
   "have headers when method is OPTIONS" >> {
     val req = Request(httpOptionsRequest)
     val res = Response(new MockHttpServletResponse)
     dispatch(new Resource with TestResource, req, res)     
     res.status must beEqualTo("200")
     res.body must beEqualTo("")
     res.headers must havePair(("X-mas", "happy holiday"))
   }
   
   "have non unicode body" >> {
     trait NonUnicode extends Resource {
       override def to_html(request: Request, response: Response) = {
         response.charset = None
         "hi" 
       }       
     }
     val req = Request(httpGetRequest)
     val res = Response(new MockHttpServletResponse)
     dispatch(new Resource with TestResource with NonUnicode, req, res)
     res.status must beEqualTo("200")
     res.body must beEqualTo("hi")
   }
      
 }
}