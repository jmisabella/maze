package maze.classes

import play.api.libs.json.{ Json, Format, JsSuccess }
import com.fasterxml.jackson.core.JsonParseException

case class SerializedCoordinates(x: String, y: String) {
  override def toString(): String = (Json.obj("x" -> x, "y" -> y)).toString()
}
object SerializedCoordinates {
  implicit val format: Format[SerializedCoordinates] = Json.format[SerializedCoordinates]
}
case class Coordinates(x: Int, y: Int) {
  override def toString(): String = (Json.obj("x" -> x, "y" -> y)).toString()
}
object Coordinates {
  implicit def ordering [A <: Coordinates]: Ordering[A] = Ordering.by(c => (c.x, c.y))
  implicit val format: Format[Coordinates] = Json.format[Coordinates]
  
  def apply(coords: SerializedCoordinates): Coordinates = Coordinates(coords.x.toInt, coords.y.toInt)

  def apply(json: String): Coordinates = try {
    Json.parse(json).validate[Coordinates] match {
      case JsSuccess(coords, _) => coords 
      case e => throw new IllegalArgumentException("Error occurred deserializing json into Coordinates") 
    }
  } catch {
    case _: Exception => try {
      Json.parse(json).validate[SerializedCoordinates] match {
        case JsSuccess(coords, _) => Coordinates(coords) 
        case e => throw new IllegalArgumentException("Error occurred deserializing json into SerializedCoordinates") 
      } 
    } catch {
      case e: JsonParseException => throw e
    }
  }
}
