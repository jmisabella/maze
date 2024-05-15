package maze.classes

object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West = Value
  
  def fromString(s: String): Option[Direction] = values.find(_.toString == s)
}
import Direction._

case class Coordinates(x: Int, y: Int) {
  override def toString(): String = s"($x,$y)"
}
object Coordinates {
  implicit def ordering [A <: Coordinates]: Ordering[A] = Ordering.by(c => (c.x, c.y))
}
case class Neighbors(
  north: Option[Coordinates] = None,
  east: Option[Coordinates] = None,
  south: Option[Coordinates] = None,
  west: Option[Coordinates] = None,
  northeast: Option[Coordinates] = None,
  southeast: Option[Coordinates] = None,
  southwest: Option[Coordinates] = None,
  northwest: Option[Coordinates] = None
) {
  override def toString(): String = 
    s"""north: ${north.getOrElse("NONE")}, east: ${east.getOrElse("NONE")}, south: ${south.getOrElse("NONE")}, west: ${west.getOrElse("NONE")}, northeast: ${northeast.getOrElse("NONE")}, southeast: ${southeast.getOrElse("NONE")}, southwest: ${southwest.getOrElse("NONE")}, northwest: ${northwest.getOrElse("NONE")}"""
}

case class Cell(
  coords: Coordinates, 
  neighbors: Neighbors = Neighbors(), 
  linked: Set[Coordinates] = Set(),
  visited: Boolean = false
) {
  def isLinked(cell: Cell, bidi: Boolean = true): Boolean = bidi match {
    case false => linked.contains(cell.coords)
    case true => linked.contains(cell.coords) && cell.linked(this.coords)
  }
  def isLinked(coords: Coordinates): Boolean = linked.contains(coords)

  def isLinked(direction: Direction): Boolean = direction match {
    case North => neighbors.north.isDefined && isLinked(neighbors.north.get)
    case East => neighbors.east.isDefined && isLinked(neighbors.east.get)
    case South => neighbors.south.isDefined && isLinked(neighbors.south.get)
    case West => neighbors.west.isDefined && isLinked(neighbors.west.get)
  }

//   override def toString(): String = 
// s"""coords: $coords
// visited: ${visited}
// neighbors: ${neighbors}
// linked: ${linked}
// """
  override def toString(): String = 
s"""coords: $coords
linked: ${linked}
"""
}

object Cell {
  def apply(row: Int, column: Int): Cell = Cell(coords = Coordinates(row, column))
  implicit def ordering [A <: Cell]: Ordering[A] = Ordering.by(_.coords)
}
