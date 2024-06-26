package maze.classes

import play.api.libs.json.{ Json, Format }

case class Coordinates(x: Int, y: Int) {
  // override def toString(): String = s"($x,$y)"
  override def toString(): String = {
    (Json.obj(
      "x" -> x,
      "y" -> y
    )).toString()
  } 
}
object Coordinates {
  implicit def ordering [A <: Coordinates]: Ordering[A] = Ordering.by(c => (c.x, c.y))
  implicit val format: Format[Coordinates] = Json.format[Coordinates]
}
