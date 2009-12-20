package webmachine

import javax.servlet.http.HttpServletRequest

object Request {
  def apply(base : HttpServletRequest) = new Request(base)
}

class Request(val base: HttpServletRequest) {
  val method = base.getMethod
  val if_unmodified_since = base.getDateHeader("if-unmodified-since")
  val if_modified_since = base.getDateHeader("if-modified-since")
  val content_length = base.getContentLength
  val content_type: Option[String] = {
    if(base.getContentType() == null) None
    else Some(base.getContentType())
  }
  
  def hasHeader(name:String) = header(name) != null
  def header(name:String) = base.getHeader(name)
  
  def accept_best_match(content_types: List[String]):Option[String] = {
    val accept = base.getHeader("accept")
    content_types.find { accept.contains(_) } 
  }
  
  def accept_language_best_match(langs: List[String]): Option[String] = {
    val accept = base.getHeader("accept-language")
    langs.find { accept.contains(_) }    
  }
  
  def accept_charset_best_match(charsets: List[String]): Option[String] = {
    val accept = base.getHeader("accept-charset")
    charsets.find { accept.contains(_) }    
  }
  
  def accept_encoding_back_match(encodings: List[String]): Option[String] = {
    val accept = base.getHeader("accept-encoding")
    encodings.find { accept.contains(_) }    
  }
  
}