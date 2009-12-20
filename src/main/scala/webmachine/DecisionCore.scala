package webmachine

object DecisionCore {
  def b03(resource: Resource, req: Request, res:Response) = {
    "Options?"
    req.method match {
      case "OPTIONS" => {
        resource.options(req, res).foreach {
          res.headers += _
        }
        200
      } 
      case _ => c03(resource, req, res)
    }
  }
  
  def b04(resource: Resource, req: Request, res:Response) = {
    "Request entity too large?"
    resource.valid_entity_length(req, res) match {
      case true => b03(resource, req, res)
      case false => "413"
    }
  }
  
  def b05(resource: Resource, req: Request, res:Response) = {
    "Unknown Content-Type?"
    resource.known_content_type(req, res) match {
      case true => b04(resource, req, res)
      case false => "415" 
    }
  }
  
  def b06(resource: Resource, req: Request, res:Response) = {
    "Unknown or unsupported Content-* header?"
    resource.valid_content_headers(req, res) match {
      case true => b05(resource, req, res)
      case _ => "501"
    }
  }
  
  def b07(resource: Resource, req: Request, res:Response) = {
    "Forbidden?"
    resource.forbidden(req, res) match {
      case true => "403"
      case _ => b06(resource, req, res)
    }
    
  }
  
  def b08(resource: Resource, req: Request, res:Response) = {
    "Authorized?"
    resource.is_authorized(req, res) match {
      case result: boolean => result match {
        case true => b07(resource, req, res)
        case false => "401"        
      } 
      case msg: String => res.headers("WWW-Authenticate") = msg; 401
    }
  }
  
  def b09(resource: Resource, req: Request, res:Response) = {
    "Malformed?"
    resource.malformed_request(req, res) match {
      case true => "400"
      case _ => b08(resource, req, res)
    }
    
  }
  
  def b10(resource: Resource, req: Request, res:Response) = {
    "Is method allowed?"
    resource.allowed_methods(req, res) exists { req.method == _ } match {
      case true => b09(resource, req, res)
      case _ => res.allowed = resource.allowed_methods(req, res); "405"
    }
  }
  
  def b11(resource: Resource, req: Request, res:Response) = {
    "URI too long?"
    resource.uri_too_long(req, res) match {
      case true => "414"
      case _ => b10(resource, req, res)
    }
    
  }
  
  def b12(resource: Resource, req: Request, res:Response) = {
    "Known method?"
    resource.known_methods(req, res) exists {req.method == _ } match {
      case true => b11(resource, req, res)
      case _ => "501"
    }
  }
  def b13(resource: Resource, req: Request, res:Response) = {
    "Service available"
    resource.ping(req, res) && resource.service_available(req, res) match {
      case true => b12(resource, req, res)
      case _ => "503"
    }
  }
  
  def c03(resource: Resource, req: Request, res:Response) = {
    "Accept exists?"
    req.hasHeader("accept") match {
      case true => c04(resource, req, res)
      case false => d04(resource, req, res)
    }
  }
  
  def c04(resource: Resource, req: Request, res:Response) = {
    "Acceptable media type available?"
    val content_types = resource.content_types_provided(req, res) map { _._1 }
    req.accept_best_match(content_types) match {
      case Some(content_type) => {
        res.content_type = content_type
        d04(resource, req, res)
      }
      case None => "406"
    }
  }

  def d04(resource: Resource, req: Request, res:Response) = {
    "Accept-Language exists?"
    req.hasHeader("accept-language") match {
      case true => d05(resource, req, res)
      case false => e05(resource, req, res)
    }
  }
  
  def d05(resource: Resource, req: Request, res:Response) = {
    "Accept-Language available?"
    resource.languages_provided(req, res) match {
      case Some(langs) => req.accept_language_best_match(langs) match {
        case Some(matched_lang) => {
          res.content_language = Some(matched_lang)
          e05(resource, req, res)
        }
        case None => "406"
      }
      case None => e05(resource, req, res)
    }
  }
  
  def e05(resource: Resource, req: Request, res:Response) = {
    "Accept-Charset exists?"
    req.hasHeader("accept-charset") match {
      case true => e06(resource, req, res)
      case false => f06(resource, req, res)
    }
  }
  
  def e06(resource: Resource, req: Request, res:Response) = {
    "Acceptable charset available?"
    resource.charsets_provided(req, res) match {
      case Some(charsets) => req.accept_charset_best_match(charsets) match {
        case Some(matched_charset) => {
          res.charset = Some(matched_charset)
          f06(resource, req, res)
        }
        case None => "406"
      }
      case None => f06(resource, req, res)
    }
  }
  
  def f06(resource: Resource, req: Request, res:Response) = {
    "Accept-Encoding exists?"
     req.hasHeader("accept-encoding") match {
       case true => f07(resource, req, res)
       case false => g07(resource, req, res)
     }
  }
  
  def f07(resource: Resource, req: Request, res:Response) = {
    "Acceptable encoding available?"
    resource.encodings_provided(req, res) match {
      case Some(encoding_mapping) => req.accept_encoding_back_match(encoding_mapping map {_._1}) match {
        case Some(matched_encoding) => {
          res.content_encoding = Some(matched_encoding)
          g07(resource, req, res)
        }  
        case None => "406"
      }
      case None => g07(resource, req, res)
    }
  }
  
  def g07(resource: Resource, req: Request, res:Response) = {
    "Resource exists?"
    // Set variances now that conneg is done
    val variances = accept_maybe(resource, req, res) ++ 
    accept_charset_maybe(resource, req, res) ++
    accept_encoding_maybe(resource, req, res) ++
    accept_languages_maybe(resource, req, res) ++
    resource.variances(req, res)
    res.headers = res.headers + ("Vary" -> variances.mkString(", ")) 
    resource.resource_exists(req, res) match {
      case true => g08(resource, req, res)
      case false => h07(resource, req, res)
    }
  }
  
  def g08(resource: Resource, req: Request, res:Response) = {
    "If-Match exists?"
    req.hasHeader("if-match") match {
      case true => g09(resource, req, res)
      case false => h10(resource, req, res)
    }
  }
  
  def g09(resource: Resource, req: Request, res:Response) = {
    "If-Match: * exists?"
    req.header("if-match") match {
      case "*" => h10(resource, req, res)
      case _ => g11(resource, req, res)
    }
  }
  
  def g11(resource: Resource, req: Request, res:Response) = {
    "Etag in If-Match?"
    resource.generate_etag(req, res) match {
      case Some(eTag) => eTag == req.header("if-match") match {
        case true => h10(resource, req, res)
        case false => "412"
      }
      case None => "412"
    }
  }
  
  def h07(resource: Resource, req: Request, res:Response) = {
    "If-Match: * exists?"
    req.header("if-match") match {
      case "*" => "412"
      case _ => i07(resource, req, res)
    }
  }
  
  def h10(resource: Resource, req: Request, res:Response) = {
    "If-Unmodified-Since exists?"
    req.hasHeader("if-unmodified-since") match { 
      case true => h11(resource, req, res)
      case false => i12(resource, req, res)
    }
  }
  
  def h11(resource: Resource, req: Request, res:Response) = {
    "If-Unmodified-Since is a valid date?"
    req.if_unmodified_since match {
      case -1 => i12(resource, req, res)
      case _  => h12(resource, req, res)
    }
  }
  
  def h12(resource: Resource, req: Request, res:Response) = {
    "Last-Modified > If-Unmodified-Since?"
    res.last_modified = resource.last_modified(req, res) 
    res.last_modified match {
      case Some(date) => { 
        if(date > req.if_unmodified_since) "412"
        else i12(resource, req, res)
      }
      case None => i12(resource, req, res)
    }
  }
  
  def i04(resource: Resource, req: Request, res:Response) = {
    "Apply to a different URI?"
    res.location = resource.moved_permanently(req, res) 
    res.location match {
      case Some(uri) => "301"
      case None => p03(resource, req, res)
    }    
  }
  
  def i07(resource: Resource, req: Request, res:Response) = {
    "PUT?"
    req.method match {
      case "PUT" => i04(resource, req, res)
      case _ => k07(resource, req, res)
    }
  }
  
  def i12(resource: Resource, req: Request, res:Response) = {
    "If-None-Match exists?"
     req.hasHeader("if-none-match") match { 
       case true => i13(resource, req, res)
       case false => l13(resource, req, res)
     }
  }
  
  def i13(resource: Resource, req: Request, res:Response) = {
    "If-None-Match: * exists?"
    req.header("if-none-match") match {
      case "*" => j18(resource, req, res)
      case _ => k13(resource, req, res)
    }
  }
  
  def j18(resource: Resource, req: Request, res:Response) = {
    "GET/HEAD?"
    req.method match {
      case "GET" | "HEAD"=> "304"
      case _ => "412"
    }
  }
  
  def k05(resource: Resource, req: Request, res:Response) = {
    "Resource moved permanently?"
    res.location = resource.moved_permanently(req, res) 
    res.location match {
      case Some(uri) => "301"
      case None => l05(resource, req, res)
    }
  }
  
  def k07(resource: Resource, req: Request, res:Response) = {
    "Resource previously existed?"
    resource.previously_existed(req, res)  match {
      case true => k05(resource, req, res)
      case false => l07(resource, req, res)
    }
  }
  
  def k13(resource: Resource, req: Request, res:Response) = {
    "Etag in If-None-Match?"
    res.etag = resource.generate_etag(req, res) 
    res.etag match {
      case Some(eTag) => eTag == req.header("if-none-match") match {
        case true => j18(resource, req, res)
        case false => l13(resource, req, res)
      }
      case None => l13(resource, req, res)
    }
  }
  
  def l05(resource: Resource, req: Request, res:Response) = {
    "Resource moved temporarily?"
    res.location = resource.moved_temporarily(req, res) 
    res.location match {
      case Some(uri) => "307"
      case None => m05(resource, req, res)
    }
  }
  
  def l07(resource: Resource, req: Request, res:Response) = {
    "POST?"
    req.method match {
      case "POST" => m07(resource, req, res)
      case _ => "404"
    }
  }
  
  def l13(resource: Resource, req: Request, res:Response) = {
    "If-Modified-Since exists?"
    req.hasHeader("if-modified-since") match { 
       case true => l14(resource, req, res)
       case false => m16(resource, req, res)
     }
  }
  
  def l14(resource: Resource, req: Request, res:Response) = {
    "If-Modified-Since is a valid date?"
    req.if_modified_since match {
       case -1 => m16(resource, req, res)
       case _  => l15(resource, req, res)
     }
  }
  
  def l15(resource: Resource, req: Request, res:Response) = {
    "If-Modified-Since > Now?"
    val now = System.currentTimeMillis()
    if(req.if_modified_since > now) m16(resource, req, res) 
    else l17(resource, req, res)
  }
  
  def l17(resource: Resource, req: Request, res:Response) = {
    "Last-Modified > If-Modified-Since?"
    res.last_modified = resource.last_modified(req, res) 
    res.last_modified match {
      case Some(date) => {
        if(date > req.if_modified_since) m16(resource, req, res)
        else "304"
      }
      case None => "304" 
    }
  }
  
  def m05(resource: Resource, req: Request, res:Response) = {
    "POST?"
    req.method match {
      case "POST" => n05(resource, req, res)
      case _ => "410"
    }
  }
  
  
  def m07(resource: Resource, req: Request, res:Response) = {
    "Server permits POST to missing resource?"
    resource.allow_missing_post(req, res) match {
      case true => n11(resource, req, res)
      case false => "404"
    }
  }
  
  def m16(resource: Resource, req: Request, res:Response) = { 
    "DELETE?"
    req.method match {
      case "DELETE" => m20(resource, req, res)
      case _ => n16(resource, req, res)
    }
  }
  
  def m20(resource: Resource, req: Request, res:Response) = {
    "Delete enacted?"
    resource.delete_completed(req, res) match {
      case true => o20(resource, req, res)
      case false => "202"
    }
  }
  
  def n05(resource: Resource, req: Request, res:Response) = {
    "Server permits POST to missing resource?"
    resource.allow_missing_post(req, res) match {
      case true => n11(resource, req, res)
      case false => "410"
    }
  }
  
  def n11(resource: Resource, req: Request, res:Response) = {
    "Redirect?"
    resource.post_is_create(req, res) match {
      case true => {
        handle_request_body(resource, req, res)
        res.location = resource.created_location(req, res) 
        res.location match {
          case Some(uri) => "303"
          case None => p11(resource, req, res)
        }
      }
      case false => {
        if(!resource.process_post(req, res)) throw new RuntimeException("Failed to process POST")
        p11(resource, req, res)
      }
    }
  }
  
  def n16(resource: Resource, req: Request, res:Response) = {
    "POST?"
    req.method match {
      case "POST" => n11(resource, req, res)
      case _ => o16(resource, req, res)
    }
  }
  
  def o14(resource: Resource, req: Request, res:Response) = {
    "Is conflict?"
    resource.is_conflict(req, res) match {
      case true => "409"
      case false => p11(resource, req, res)
    }
  }
  
  def o16(resource: Resource, req: Request, res:Response) = {
    "PUT?"
    req.method match {
      case "PUT" => o14(resource, req, res)
      case _ => o18(resource, req, res)
    }
  }
  
  
  def o18(resource: Resource, req: Request, res:Response) = {
    "Multiple representations? (Build GET/HEAD body)"
    req.method match {
      case "GET" | "HEAD" => {
        handle_response_body(resource, req, res)
        if(resource.multiple_choices(req, res)) "300"
        else "200"
      }
      case _ => { 
        if(resource.multiple_choices(req, res)) "300"
        else "200"
      }
    }
  }
  
  def o20(resource: Resource, req: Request, res:Response) = {
    "Response includes entity?"
    res.body match {
      case "" => "204"
      case _ => o18(resource, req, res)
    }
    
  }
  def p03(resource: Resource, req: Request, res:Response) = {
    "Conflict?"
    resource.is_conflict(req, res) match {
      case true => "409"
      case false => {
        handle_request_body(resource, req, res)
        p11(resource, req, res)
      }
    }  
  }
  
  def p11(resource: Resource, req: Request, res:Response) = {
    "New resource?"
    res.location match {
      case Some(uri) => "201"
      case None => o20(resource, req, res)
    }
  }
  
  def handle_request(resource: Resource, req: Request, res:Response) {
    val content_types = resource.content_types_provided(req, res)
    if(content_types.size > 0) {
      res.content_type = content_types.head._1
    }
    res.status = b13(resource, req, res).toString
  }
  
  private def handle_request_body(resource: Resource, req: Request, res: Response) = {
    val content_type = req.content_type match {
      case Some(ctype) => ctype.split(";", 1)(0)
      case None => "application/octet-stream"
    }
    resource.content_types_provided(req, res).filter { _._1 == content_type } match {
      case Nil => throw new RuntimeException("Resource does not support the requested content type")
      case List(matching_content_mapping) => matching_content_mapping._2(req, res)
    } 
  }
  
  private def handle_response_body(resource: Resource, req: Request, res: Response) = {    
    res.etag = resource.generate_etag(req, res)
    res.last_modified = resource.last_modified(req, res)
    res.expires = resource.expires(req, res)

    val body = resource.content_types_provided(req, res).find { _._1 == res.content_type } match {
      case None => throw new RuntimeException("Resource does not support the requested content type")
      case Some(matching_content_mapping) => matching_content_mapping._2(req, res)
    } 
    res.charset match {
      case Some(charset) => res.body = unicode(body)
      case None => res.body = body
    }
    
    if(res.content_encoding.isDefined) {
      resource.encodings_provided(req, res) match {
        case Some(encoding_mappings) => {
          encoding_mappings.find { _._1 == res.content_encoding.get } match {
            case None => throw new RuntimeException("Resource does not support the requested encoding")
            case Some(matching_encoding_mapping) => res.body = matching_encoding_mapping._2(res.body)        
          }
        }
        case None => throw new RuntimeException("Resource does not support the requested encoding")
      }
    } 
  }
  
  private def unicode(s: String) = {
    new String(s.getBytes("UTF-8"))
  }
  
  private def accept_maybe(resource:Resource, req: Request, res:Response) = {
    resource.content_types_provided(req, res).size match {
      case 0 | 1 => Nil
      case _ => "Accept" :: Nil
    }
  }
  
  private def accept_charset_maybe(resource:Resource, req: Request, res:Response) = {
    resource.charsets_provided(req, res) match {
      case Some(charsets) => charsets.size match {
        case 0 | 1 => Nil
        case _ => "Accept-Charset" :: Nil        
      }
      case None => Nil
    }
  }
  
  private def accept_encoding_maybe(resource:Resource, req: Request, res:Response) = {
    resource.encodings_provided(req, res) match {
      case Some(encodings) => encodings.size match {
        case 0 | 1 => Nil
        case _ => "Accept-Encoding" :: Nil        
      }
      case None => Nil
    }
  }
  
  private def accept_languages_maybe(resource:Resource, req: Request, res:Response) = {
    resource.languages_provided(req, res) match {
      case Some(languages) => languages.size match {
        case 0 | 1 => Nil
        case _ => "Accept-Language" :: Nil        
      }
      case None => Nil
    }
  }
  
  
}