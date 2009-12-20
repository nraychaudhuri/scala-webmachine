package webmachine.sample

import webmachine._

class SampleResource extends Resource {
  override def to_html(request: Request, response: Response) = <b>hello world</b>.toString
}

