package maze.behaviors

import maze.behaviors.{ Cell, Grid }
import maze.classes.{ Coordinates }
import maze.classes.MazeType._
import maze.classes.direction.SquareDirection._
import scala.reflect.ClassTag

trait Linkage[C <: Cell, G <: Grid[C]] {


  def visit(cell: C): C = cell.visit(visited = true)
  
  def linked(cell1: C, cell2: C, bidi: Boolean = true): Boolean = bidi match {
    // case false => cell1.linked.contains(cell2.coords)
    // case true => cell1.linked.contains(cell2.coords) && cell2.linked.contains(cell1.coords) 
    case _ => cell1.linked.contains(cell2.coords) && cell2.linked.contains(cell1.coords) 
  }
  def linkAll(cells: Seq[C], bidi: Boolean, f: (C, C, Boolean) => Seq[C])(implicit ct: ClassTag[C]): Seq[C] = {
    val ungrouped: Seq[C] = (for ((c1, c2) <- cells zip cells.drop(1)) yield f(c1, c2, bidi)).flatten
    val grouped = ungrouped.groupBy(c => (c.coords, c.visited, c.neighborsByDirection, c.mazeType))
    val merged: Seq[Option[C]] = grouped.foldLeft(Nil: Seq[Option[C]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        // val neighbors: N = k._3.asInstanceOf[N]
        val neighborsByDirection: Map[String, Coordinates] = k._3
        val mazeType: MazeType = k._4
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        val cell: C = Cell.instantiate[C](mazeType, coords, visited, neighborsByDirection, linked)
        acc ++ Seq(Some(cell))
      }
    }
    merged.filter(_.isDefined).map(_.get)
  }
  def link(cell1: C, cell2: C, bidi: Boolean)(implicit ct: ClassTag[C]): Seq[C] = bidi match {
    case _ => Seq(cell1.setLinked[C](cell1.linked + cell2.coords), cell2.setLinked[C](cell2.linked + cell1.coords))
  }
  def link(cells: Seq[C], bidi: Boolean = true)(implicit ct: ClassTag[C]): Seq[C] = linkAll(cells, bidi, link)

  def link(cell1: C, cell2: C, grid: G)(implicit ct: ClassTag[C]): G = {
    if (cell1.neighbors().contains(cell2.coords) && cell2.neighbors().contains(cell1.coords)) {
      val updated1: C = cell1.setLinked[C](cell1.linked ++ Set(cell2.coords))
      val updated2: C = cell2.setLinked[C](cell2.linked ++ Set(cell1.coords))
      grid.set[G](updated1).set(updated2)
    } else {
      grid
    }
  }

  //// TODO: fix actual bug causing unidirectional linking so we can remove this bandaid method
  def repairUniDirectonalLinks(grid: G)(implicit ct: ClassTag[C]): G = {
    var updated: G = grid
    for (y <- 0 until grid.height) {
      val row: Seq[C] = updated.row(y)
      for (cell <- row) {
        if (cell.coords.x < grid.width - 1) {
          val neighbor: C = updated.get(cell.neighbors(East).head)
          if (cell.linked.contains(neighbor.coords) && !neighbor.linked.contains(cell.coords)) {
            updated = updated.set(neighbor.setLinked(neighbor.linked ++ Set(cell.coords))) 
          } else if (!cell.linked.contains(neighbor.coords) && neighbor.linked.contains(cell.coords)) {
            updated = updated.set(cell.setLinked(cell.linked ++ Set(neighbor.coords))) 
          }
        }
      }
    }
    for (x <- 0 until grid.width) {
      val column: Seq[C] = grid.column(x)
      for (cell <- column) {
        if (cell.coords.y < grid.height - 1) {
          val neighbor: C = updated.get(cell.neighbors(South).head)
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
