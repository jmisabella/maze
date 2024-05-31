package maze.classes

object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West = Value
  
  def fromString(s: String): Option[Direction] = values.find(_.toString == s)
}
