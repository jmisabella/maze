package maze.classes

import maze.classes.Algorithm
import maze.classes.Algorithm._
import play.api.libs.json.{ Json, Format, JsSuccess }
import com.fasterxml.jackson.core.JsonParseException

private case class SerializedMazeRequest(width: String, height: String, algorithm: String) {
  override def toString(): String = (Json.obj("width" -> width, "height" -> height, "algorithm" -> algorithm)).toString()
}
private object SerializedMazeRequest {
  implicit val format: Format[SerializedMazeRequest] = Json.format[SerializedMazeRequest]
}
case class MazeRequest(width: Int, height: Int, algorithm: Algorithm) {
  override def toString(): String = (Json.obj("width" -> width, "height" -> height, "algorithm" -> algorithm)).toString()
}
object MazeRequest {
  implicit val format: Format[MazeRequest] = Json.format[MazeRequest]
  
  def apply(req: SerializedMazeRequest): MazeRequest = { 
    MazeRequest(
      req.width.toInt, 
      req.height.toInt, 
      Algorithm.fromString(req.algorithm).getOrElse(
        throw new IllegalArgumentException(s"Unexpected algorithm [${req.algorithm}] provided in request")))
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