package webmachine

class Resource {
  def to_html(request: Request, response: Response) = { "Individual Resource should override this default to_html method " }
  
  def content_types_provided(req: Request, res: Response) = List(("text/html", to_html _))

  def allowed_methods(req: Request, res: Response) = List("GET", "HEAD")

  def allow_missing_post(req: Request, res: Response) = false

  def auth_required(req: Request, res: Response) = true

  def charsets_provided(req: Request, res: Response):Option[List[String]] = None

  def content_types_accepted(req: Request, res: Response) = None

  def created_location(req: Request, res: Response): Option[String] = None

  def delete_completed(req: Request, res: Response) = true
  
  def delete_resource(req: Request, res: Response) = false

  def encodings_provided(req: Request, res: Response):Option[List[(String, (String) => String)]] = None

  def expires(req: Request, res: Response): Option[Long]  = None
  
  def finish_request(req: Request, res: Response) = true
  
  def forbidden(req: Request, res: Response) = false
  
  def generate_etag(req: Request, res: Response): Option[String] = None

  def is_authorized(req: Request, res: Response): Any = true
  
  def is_conflict(req: Request, res: Response) = false

  def known_content_type(req: Request, res: Response) = true

  def known_methods(req: Request, res: Response) = List("GET", "HEAD", "POST", "PUT", "DELETE",
          "TRACE", "CONNECT", "OPTIONS")

  def languages_provided(req: Request, res: Response):Option[List[String]] = None

  def last_modified(req: Request, res: Response):Option[Long] = None

  def malformed_request(req: Request, res: Response) = false

  def moved_permanently(req: Request, res: Response):Option[String] = None

  def moved_temporarily(req: Request, res: Response): Option[String] = None

  def multiple_choices(req: Request, res: Response) = false

  def options(req: Request, res: Response):List[(String, String)] = Nil

  def ping(req: Request, res: Response) = true

  def post_is_create(req: Request, res: Response) = false
  
  def previously_existed(req: Request, res: Response) = false

  def process_post(req: Request, res: Response) = false

  def resource_exists(req: Request, res: Response) = true
  
  def service_available(req: Request, res: Response) = true

  def uri_too_long(req: Request, res: Response)= false
  
  def valid_content_headers(req: Request, res: Response)= true
  
  def valid_entity_length(req: Request, res: Response) = true

  def variances(req: Request, res: Response): List[String] = Nil
}