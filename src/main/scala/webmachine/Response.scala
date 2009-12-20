package webmachine
import javax.servlet.http.HttpServletResponse

object Response {
  def apply(base: HttpServletResponse) = new Response(base)
}
class Response(val base:HttpServletResponse) {  
  var content_language: Option[String] = None
  var content_encoding:Option[String] = None  
  var last_modified:Option[Long] = None
  var expires: Option[Long] = None
  var charset:Option[String] = None
  var location:Option[String] = None
  var etag: Option[String] = None
  var allowed:List[String] = Nil
  var headers:Map[String, String] = Map()
  
  var status:String = _
  var body:String = ""
  var content_type:String = _
  
  //TODO: find a better way to update the base response object 
  def flush = {
    base.setStatus(status.toInt)
    if(content_language.isDefined) base.addHeader("Content-Language", content_language.get)
    if(content_encoding.isDefined) base.addHeader("Content-Encoding", content_encoding.get)
    if(last_modified.isDefined) base.addDateHeader("Last-Modified", last_modified.get)
    if(expires.isDefined) base.addDateHeader("Expires", expires.get)
    charset match {
      case Some(char_encoding) =>  base.setContentType(content_type + "; charset=" + char_encoding)
      case None => base.setContentType(content_type)
    }
    //TODO: do we have to encode the url here? Not sure whether supporting the session yet
    if(location.isDefined) base.sendRedirect(location.get)
    if(etag.isDefined) base.addHeader("ETag", etag.get)
    base.addHeader("Allow", allowed.mkString(", "))
    headers.foreach { (t: (String, String)) => base.addHeader(t._1, t._2) }
    base.getOutputStream.println(body)
    base.flushBuffer
  }
}