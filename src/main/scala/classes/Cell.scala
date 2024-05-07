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
  def visit(): Cell = copy(visited = true)
  def unvisit(): Cell = copy(visited = false)
  def link(cell: Cell, bidi: Boolean = true): Seq[Cell] = bidi match {
    case false => Seq(this.copy(linked = this.linked + cell.coords))
    case true => Seq(this.copy(linked = this.linked + cell.coords), cell.copy(linked = cell.linked + this.coords))
  }
  def unlink(cell: Cell, bidi: Boolean = true): Seq[Cell] = bidi match {
    case false => Seq(this.copy(linked = this.linked - cell.coords))
    case true => Seq(this.copy(linked = this.linked - cell.coords), cell.copy(linked = cell.linked - this.coords))
  }
  def isLinked(cell: Cell): Boolean = linked.contains(cell.coords)

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
