package maze.classes

import maze.classes.Algorithm._
import maze.classes.MazeType._
import maze.classes.{ Coordinates, SerializedCoordinates } 
import play.api.libs.json.{ Json, Format, JsSuccess }
import com.fasterxml.jackson.core.JsonParseException

private case class SerializedMazeRequest(width: String, height: String, algorithm: String, startX: String, startY: String, goalX: String, goalY: String, mazeType: String = "Solved") {
  override def toString(): String = {
    (Json.obj(
      "width" -> width, 
      "height" -> height, 
      "algorithm" -> algorithm, 
      "mazeType" -> mazeType,
      "start" -> SerializedCoordinates(startX, startY), 
      "goal" -> SerializedCoordinates(goalX, goalY))).toString()
  }
}
private object SerializedMazeRequest {
  implicit val format: Format[SerializedMazeRequest] = Json.format[SerializedMazeRequest]
}
case class MazeRequest(width: Int, height: Int, algorithm: Algorithm, start: Coordinates, goal: Coordinates, mazeType: MazeType = Solved) {
  override def toString(): String = (Json.obj("width" -> width, "height" -> height, "algorithm" -> algorithm, "mazeType" -> mazeType, "start" -> start, "goal" -> goal)).toString()
}
object MazeRequest {
  implicit val format: Format[MazeRequest] = Json.format[MazeRequest]
  
  def apply(req: SerializedMazeRequest): MazeRequest = { 
    MazeRequest(
      req.width.toInt, 
      req.height.toInt, 
      Algorithm.fromString(req.algorithm).getOrElse(
        throw new IllegalArgumentException(s"Unexpected algorithm [${req.algorithm}] provided in request")),
      Coordinates(SerializedCoordinates(req.startX, req.startY)),
      Coordinates(SerializedCoordinates(req.goalX, req.goalY)),
      MazeType.fromString(req.mazeType).getOrElse(Solved))
  } 
  
  def apply(json: String): MazeRequest = try {
    Json.parse(json).validate[MazeRequest] match {
      case JsSuccess(req, _) => req
      case e => throw new IllegalArgumentException("Error occurred deserializing request into a MazeRequest") 
    }
  } catch {
    case e: Exception => try {
      Json.parse(json).validate[SerializedMazeRequest] match {
        case JsSuccess(req, _) => MazeRequest(req) 
        case e => throw new IllegalArgumentException("Error occurred deserializing request into a SerializedMazeRequest") 
      } 
    } catch {
      case e: JsonParseException => throw e
    }
  }

}