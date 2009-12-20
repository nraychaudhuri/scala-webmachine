package webmachine
import scala.collection.mutable.{Map => MM}
object Route {
  val routes = MM[String, Resource]()
  def apply(path:String, resource:Resource) = routes + (path -> resource) 
}