package webmachine

object Dispatcher {
  def d(requestUri: String, req: Request, res: Response) = {
    dispatch(Route.routes(requestUri), req, res)
  }
  def dispatch(resource: Resource, req: Request, res: Response) = {
    DecisionCore.handle_request(resource, req, res)
  }

}