package maze.classes.direction

object SquareDirection extends Enumeration {
  type SquareDirection = Value
  val North, East, South, West = Value
  
  def fromString(s: String): Option[SquareDirection] = values.find(_.toString.toLowerCase == s.toLowerCase)
}