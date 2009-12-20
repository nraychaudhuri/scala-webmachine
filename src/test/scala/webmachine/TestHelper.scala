package webmachine

import org.springframework.mock.web._ 

object TestHelper {
  
  def httpGetRequest = {
    val httpRequest = new MockHttpServletRequest
    httpRequest.setMethod("GET")
    httpRequest
  }
  
  def httpPostRequest = {
    val httpRequest = new MockHttpServletRequest
    httpRequest.setMethod("POST")
    httpRequest
  }
  
  def httpPutRequest = {
    val httpRequest = new MockHttpServletRequest
    httpRequest.setMethod("PUT")
    httpRequest
  }
  
  def httpOptionsRequest = {
    val httpRequest = new MockHttpServletRequest
    httpRequest.setMethod("OPTIONS")
    httpRequest
  }
}
