package maze.classes

import maze.classes.{ Coordinates, Neighbors }
import maze.classes.Direction._

case class Cell(
  coords: Coordinates, 
  neighbors: Neighbors = Neighbors(), 
  linked: Set[Coordinates] = Set(),
  visited: Boolean = false,
  value: String = "   "
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

  def padRight(s: String, c: Char, n: Int): String = s.padTo(n, c).mkString
  def padLeft(s: String, c: Char, n: Int): String = n match {
    case 0 => s
    case x if (x < 0) => s
    case _ => padRight(s, c, n).split(s).tail.mkString + s
  }
  // evenly pad left and right; left has 1 extra padding in case of an odd length 
  def pad(s:String, c: Char, n:Int): String = {
    val left = (n - s.length) / 2
    val right = n - left - s.length 
    c.toString * left + s + c.toString * right
  }

  override def toString(): String = 
s"""coords: $coords
linked: ${linked}
"""
  // override def toString(): String = ""

//   override def toString(): String = 
// s"""coords: $coords
// visited: ${visited}
// neighbors: ${neighbors}
// linked: ${linked}
// """

}

object Cell {
  def apply(row: Int, column: Int): Cell = Cell(coords = Coordinates(row, column))
  implicit def ordering [A <: Cell]: Ordering[A] = Ordering.by(_.coords)
}
