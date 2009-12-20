package webmachine
import org.specs._
import org.specs.runner.JUnit4
import org.springframework.mock.web._ 
import TestHelper._
import Dispatcher._
  
class b13SpecTest extends JUnit4(b13Spec)

object b13Spec extends Specification { 
  
  "webmachine" should {
    "respond with 200 when resource responds to ping" >> {
      trait TestResource extends Resource {
        override def ping(request: Request, response: Response) = true
        override def to_html(request: Request, response: Response) = "i am available"
      }
      
      val req = new Request(httpGetRequest)
      val res = Response(new MockHttpServletResponse)
      dispatch(new Resource with TestResource, req, res)
      res.status must beEqualTo("200")
      res.body must beEqualTo("i am available")
   }
              
   "respond with '503 Service Unavailable' when resource doesn't respond to ping" >> {
     trait TestResource extends Resource {
       override def ping(request: Request, response: Response) = false
     }
     val req = new Request(httpGetRequest)
     val res = Response(new MockHttpServletResponse)
     dispatch(new Resource with TestResource, req, res)
     res.status must beEqualTo("503")
     res.body must beEqualTo("")
   }
              
   "respond with '503 Service Unavailable' when resource is not available" >> {
     trait TestResource extends Resource {
       override def service_available(request: Request, response: Response) = false
     }
     val req = new Request(httpGetRequest)
     val res = Response(new MockHttpServletResponse)
     dispatch(new Resource with TestResource, req, res)
     res.status must beEqualTo("503")
     res.body must beEqualTo("")
   }           
 }
}