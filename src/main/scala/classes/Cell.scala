package maze.classes

case class Coordinates(x: Int, y: Int) {
  override def toString(): String = s"($x,$y)"
}

case class Neighbors(
  north: Option[Coordinates] = None,
  east: Option[Coordinates] = None,
  south: Option[Coordinates] = None,
  west: Option[Coordinates] = None
) {
  override def toString(): String = 
    s"""north: ${north.getOrElse("NONE")}, east: ${east.getOrElse("NONE")}, south: ${south.getOrElse("NONE")}, west: ${west.getOrElse("NONE")}"""
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

  override def toString(): String = 
s"""coords: $coords
visited: ${visited}
neighbors: ${neighbors}
linked: ${linked}
"""
}

object Cell {
  def apply(row: Int, column: Int): Cell = Cell(coords = Coordinates(row, column))
}
