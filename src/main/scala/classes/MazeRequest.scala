package maze.classes

import maze.classes.Algorithm._
import maze.classes.MazeType._
import play.api.libs.json.{ Json, Format, JsSuccess }
import com.fasterxml.jackson.core.JsonParseException

private case class SerializedMazeRequest(width: String, height: String, algorithm: String, mazeType: String = MazeType.Unsolved.toString()) {
  override def toString(): String = (Json.obj("width" -> width, "height" -> height, "algorithm" -> algorithm, "mazeType" -> mazeType)).toString()
}
private object SerializedMazeRequest {
  implicit val format: Format[SerializedMazeRequest] = Json.format[SerializedMazeRequest]
}
case class MazeRequest(width: Int, height: Int, algorithm: Algorithm, mazeType: MazeType = MazeType.Unsolved) {
  override def toString(): String = (Json.obj("width" -> width, "height" -> height, "algorithm" -> algorithm, "mazeType" -> mazeType)).toString()
}
object MazeRequest {
  implicit val format: Format[MazeRequest] = Json.format[MazeRequest]
  
  def apply(req: SerializedMazeRequest): MazeRequest = { 
    MazeRequest(
      req.width.toInt, 
      req.height.toInt, 
      Algorithm.fromString(req.algorithm).getOrElse(
        throw new IllegalArgumentException(s"Unexpected algorithm [${req.algorithm}] provided in request")),
      MazeType.fromString(req.mazeType).getOrElse(
        throw new IllegalArgumentException(s"Unexpected type [${req.mazeType}] provided in request")),
      )
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