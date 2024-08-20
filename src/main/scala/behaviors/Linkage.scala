package maze.behaviors

import maze.classes.{ Coordinates, Neighbors, Cell, Grid }

trait Linkage {

  def visit(cell: Cell): Cell = cell.copy(visited = true)
  
  def linked(cell1: Cell, cell2: Cell, bidi: Boolean = true): Boolean = bidi match {
    case false => cell1.linked.contains(cell2.coords)
    case true => cell1.linked.contains(cell2.coords) && cell2.linked.contains(cell1.coords) 
  }
  def linkAll(cells: Seq[Cell], bidi: Boolean, f: (Cell, Cell, Boolean) => Seq[Cell]): Seq[Cell] = {
    val ungrouped: Seq[Cell] = (for ((c1, c2) <- cells zip cells.drop(1)) yield f(c1, c2, bidi)).flatten
    val grouped = ungrouped.groupBy(c => (c.coords, c.visited, c.neighbors))
    val merged: Seq[Option[Cell]] = grouped.foldLeft(Nil: Seq[Option[Cell]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        val neighbors: Neighbors = k._3
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        acc ++ Seq(Some(Cell(coords = coords, visited = visited, neighbors = neighbors, linked = linked)))
      }
    }
    merged.filter(_.isDefined).map(_.get)
  }
  def link(cell1: Cell, cell2: Cell, bidi: Boolean): Seq[Cell] = bidi match {
    case false => Seq(cell1.copy(linked = cell1.linked + cell2.coords))
    case true => Seq(cell1.copy(linked = cell1.linked + cell2.coords), cell2.copy(linked = cell2.linked + cell1.coords))
  }
  def link(cells: Seq[Cell], bidi: Boolean = true): Seq[Cell] = linkAll(cells, bidi, link)

  def link(cell1: Cell, cell2: Cell, grid: Grid): Grid = {
    if (cell1.neighbors.toSeq.contains(cell2.coords) && cell2.neighbors.toSeq.contains(cell1.coords)) {
      val updated1: Cell = cell1.copy(linked = cell1.linked ++ Set(cell2.coords))
      val updated2: Cell = cell2.copy(linked = cell2.linked ++ Set(cell1.coords))
      grid.set(updated1).set(updated2) 
    } else {
      grid
    }
  }
}