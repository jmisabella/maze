package maze.classes

case class Coordinates(x: Int, y: Int) {
  override def toString(): String = s"($x,$y)"
}
object Coordinates {
  implicit def ordering [A <: Coordinates]: Ordering[A] = Ordering.by(c => (c.x, c.y))
}
