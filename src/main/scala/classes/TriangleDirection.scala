package maze.classes

object TriangleDirection extends Enumeration {
  type TriangleDirection = Value
  val UpperLeft, UpperRight, Bottom, Top, LowerLeft, LowerRight = Value
  
  def fromString(s: String): Option[TriangleDirection] = values.find(_.toString.toLowerCase == s.toLowerCase)
}
