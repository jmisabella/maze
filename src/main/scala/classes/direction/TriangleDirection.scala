package maze.classes.direction

object TriangleDirection extends Enumeration {
  type TriangleDirection = Value
  val UpperLeft, UpperRight, Down, Up, LowerLeft, LowerRight = Value
  
  def fromString(s: String): Option[TriangleDirection] = values.find(_.toString.toLowerCase == s.toLowerCase)
}
