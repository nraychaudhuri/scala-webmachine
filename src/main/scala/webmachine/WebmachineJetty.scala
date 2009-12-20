package webmachine

import java.io.IOException
import javax.servlet.ServletException
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import org.mortbay.jetty.Handler
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler

object WebmachineJettyHandler extends AbstractHandler {
  def handle(requestUri:String, httpRequest: HttpServletRequest, httpResponse:HttpServletResponse, dispatch: Int) {
    val req = Request(httpRequest)
    val res = Response(httpResponse)
    Dispatcher.d(requestUri, req, res)
    res.flush
  }  
}

object WebmachineJetty {
  def start(port:Int) = {
    val server = new Server(port)
    server.setHandler(WebmachineJettyHandler)
    server.start()
  }
}