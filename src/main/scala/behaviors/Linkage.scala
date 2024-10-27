package maze.behaviors

// import maze.behaviors.{ Cell, Grid }
import maze.classes.{ Coordinates, Cell, Grid }
import maze.classes.MazeType._
import maze.classes.direction.SquareDirection._
import scala.reflect.ClassTag

trait Linkage {


  def visit(cell: Cell): Cell = cell.visit(visited = true)
  
  def linked(cell1: Cell, cell2: Cell, bidi: Boolean = true): Boolean = bidi match {
    // case false => cell1.linked.contains(cell2.coords)
    // case true => cell1.linked.contains(cell2.coords) && cell2.linked.contains(cell1.coords) 
    case _ => cell1.linked.contains(cell2.coords) && cell2.linked.contains(cell1.coords) 
  }
  def linkAll(cells: Seq[Cell], bidi: Boolean, f: (Cell, Cell, Boolean) => Seq[Cell]): Seq[Cell] = {
    val ungrouped: Seq[Cell] = (for ((c1, c2) <- cells zip cells.drop(1)) yield f(c1, c2, bidi)).flatten
    val grouped = ungrouped.groupBy(c => (c.coords, c.visited, c.neighborsByDirection, c.mazeType))
    val merged: Seq[Option[Cell]] = grouped.foldLeft(Nil: Seq[Option[Cell]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        // val neighbors: N = k._3.asInstanceOf[N]
        val neighborsByDirection: Map[String, Coordinates] = k._3
        val mazeType: MazeType = k._4
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        val cell: Cell = Cell(coords = coords, mazeType = mazeType, visited = visited, neighborsByDirection = neighborsByDirection, linked = linked)
        acc ++ Seq(Some(cell))
      }
    }
    merged.filter(_.isDefined).map(_.get)
  }
  def link(cell1: Cell, cell2: Cell, bidi: Boolean): Seq[Cell] = bidi match {
    case _ => Seq(cell1.setLinked(cell1.linked + cell2.coords), cell2.setLinked(cell2.linked + cell1.coords))
  }
  def link(cells: Seq[Cell], bidi: Boolean = true): Seq[Cell] = linkAll(cells, bidi, link)

  def link(cell1: Cell, cell2: Cell, grid: Grid): Grid = {
    if (cell1.neighbors().contains(cell2.coords) && cell2.neighbors().contains(cell1.coords)) {
      val updated1: Cell = cell1.setLinked(cell1.linked ++ Set(cell2.coords))
      val updated2: Cell = cell2.setLinked(cell2.linked ++ Set(cell1.coords))
      grid.set(updated1).set(updated2)
    } else {
      grid
    }
  }

  //// TODO: fix actual bug causing unidirectional linking so we can remove this bandaid method
  def repairUniDirectonalLinks(grid: Grid): Grid = {
    var updated: Grid = grid
    for (y <- 0 until grid.height) {
      val row: Seq[Cell] = updated.row(y)
      for (cell <- row) {
        if (cell.coords.x < grid.width - 1) {
          val neighbor: Cell = updated.get(cell.neighbors(East).head)
          if (cell.linked.contains(neighbor.coords) && !neighbor.linked.contains(cell.coords)) {
            updated = updated.set(neighbor.setLinked(neighbor.linked ++ Set(cell.coords))) 
          } else if (!cell.linked.contains(neighbor.coords) && neighbor.linked.contains(cell.coords)) {
            updated = updated.set(cell.setLinked(cell.linked ++ Set(neighbor.coords))) 
          }
        }
      }
    }
    for (x <- 0 until grid.width) {
      val column: Seq[Cell] = grid.column(x)
      for (cell <- column) {
        if (cell.coords.y < grid.height - 1) {
          val neighbor: Cell = updated.get(cell.neighbors(South).head)
          if (cell.linked.contains(neighbor.coords) && !neighbor.linked.contains(cell.coords)) {
            updated = updated.set(neighbor.setLinked(neighbor.linked ++ Set(cell.coords))) 
          } else if (!cell.linked.contains(neighbor.coords) && neighbor.linked.contains(cell.coords)) {
            updated = updated.set(cell.setLinked(cell.linked ++ Set(neighbor.coords))) 
          }
        }
      } 
    }
    updated
  }
}
