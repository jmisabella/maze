package maze.classes

import maze.classes.Algorithm
import maze.classes.Algorithm._
import play.api.libs.json.{ Json, Format }

case class MazeRequest(width: Int, height: Int, algorithm: Algorithm) {
  override def toString(): String = (Json.obj("width" -> width, "height" -> height, "algorithm" -> algorithm)).toString()
}
object MazeRequest {
  implicit val format: Format[MazeRequest] = Json.format[MazeRequest]
}