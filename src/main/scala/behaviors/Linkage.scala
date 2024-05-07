package maze.behaviors

import maze.classes.{ Coordinates, Neighbors, Cell, Grid }

trait Linkage {

  def visit(cell: Cell): Cell = cell.copy(visited = true)
  def unvisit(cell: Cell): Cell = cell.copy(visited = false)
  def link(cell1: Cell, cell2: Cell, bidi: Boolean): Seq[Cell] = bidi match {
    case false => Seq(cell1.copy(linked = cell1.linked + cell2.coords))
    case true => Seq(cell1.copy(linked = cell1.linked + cell2.coords), cell2.copy(linked = cell2.linked + cell1.coords))
  }
  def unlink(cell1: Cell, cell2: Cell, bidi: Boolean): Seq[Cell] = bidi match {
    case false => Seq(cell1.copy(linked = cell1.linked - cell2.coords))
    case true => Seq(cell1.copy(linked = cell1.linked - cell2.coords), cell2.copy(linked = cell2.linked - cell1.coords))
  }
  def linked(cell1: Cell, cell2: Cell, bidi: Boolean = true): Boolean = bidi match {
    case false => cell1.linked.contains(cell2.coords)
    case true => cell1.linked.contains(cell2.coords) && cell2.linked.contains(cell1.coords) 
  } 
  def link(cells: Seq[Cell], bidi: Boolean = true): Seq[Cell] = {
    val bottomRowWithDupes: Seq[Cell] = (for ((c1, c2) <- cells zip cells.drop(1)) yield link(c1, c2, bidi)).flatten
    val grouped = bottomRowWithDupes.groupBy(c => (c.coords, c.visited, c.neighbors))
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

}