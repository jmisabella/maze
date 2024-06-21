package maze.behaviors.serialization

import maze.classes.MazeRequest
import maze.behaviors.serialization.Serialization
import play.api.libs.json.{ Json, JsSuccess }
import com.fasterxml.jackson.core.JsonParseException

trait MazeRequestSerialization extends Serialization[MazeRequest] {
  override def parse(json: String): Either[String, MazeRequest] = try {
    Json.parse(json).validate[MazeRequest] match {
      case JsSuccess(req, _) => Right(req)
      case e => Left(s"Error occurred: ${e.toString()}")
    }
  } catch {
    case e: JsonParseException => Left(e.getMessage())
  }
  override def json(req: MazeRequest): String = req.toString()
}
