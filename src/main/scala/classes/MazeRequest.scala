package maze.classes

import maze.classes.Algorithm._
import maze.classes.MazeType._
import maze.classes.{ Coordinates, SerializedCoordinates } 
import play.api.libs.json.{ Json, Format, JsSuccess }
import com.fasterxml.jackson.core.JsonParseException

private case class SerializedMazeRequest(mazeType: String, width: String, height: String, algorithm: String, startX: String, startY: String, goalX: String, goalY: String) {
  override def toString(): String = {
    (Json.obj(
      "mazeType" -> mazeType,
      "width" -> width, 
      "height" -> height, 
      "algorithm" -> algorithm, 
      "start" -> SerializedCoordinates(startX, startY), 
      "goal" -> SerializedCoordinates(goalX, goalY))).toString()
  }
}
private object SerializedMazeRequest {
  implicit val format: Format[SerializedMazeRequest] = Json.format[SerializedMazeRequest]
}
case class MazeRequest(mazeType: MazeType, width: Int, height: Int, algorithm: Algorithm, start: Coordinates, goal: Coordinates) {
  override def toString(): String = (Json.obj(/*"mazeType" -> mazeType,*/ "width" -> width, "height" -> height, "algorithm" -> algorithm, "start" -> start, "goal" -> goal)).toString()
}
object MazeRequest {
  implicit val format: Format[MazeRequest] = Json.format[MazeRequest]
  
  def apply(req: SerializedMazeRequest): MazeRequest = { 
    MazeRequest(
      MazeType.fromString(req.mazeType).getOrElse(
        throw new IllegalArgumentException(s"Unexpected mazeType [${req.mazeType}] provided in request")),
      req.width.toInt, 
      req.height.toInt, 
      Algorithm.fromString(req.algorithm).getOrElse(
        throw new IllegalArgumentException(s"Unexpected algorithm [${req.algorithm}] provided in request")),
      Coordinates(SerializedCoordinates(req.startX, req.startY)),
      Coordinates(SerializedCoordinates(req.goalX, req.goalY)))
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