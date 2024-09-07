package maze.behaviors

import maze.behaviors.{ Neighbors, Cell, Grid, Direction }
import maze.classes.{ Coordinates }
import maze.classes.MazeType._

import scala.reflect.ClassTag

trait Linkage[N <: Neighbors, C <: Cell, G <: Grid[C]] {

  def visit(cell: C): C = cell.visit(visited = true)
  
  def linked(cell1: C, cell2: C, bidi: Boolean = true): Boolean = bidi match {
    case false => cell1.linked.contains(cell2.coords)
    case true => cell1.linked.contains(cell2.coords) && cell2.linked.contains(cell1.coords) 
  }
  def linkAll(cells: Seq[C], bidi: Boolean, f: (C, C, Boolean) => Seq[C])(implicit ct: ClassTag[C]): Seq[C] = {
    val ungrouped: Seq[C] = (for ((c1, c2) <- cells zip cells.drop(1)) yield f(c1, c2, bidi)).flatten
    val grouped = ungrouped.groupBy(c => (c.coords, c.visited, c.neighbors, c.mazeType))
    val merged: Seq[Option[C]] = grouped.foldLeft(Nil: Seq[Option[C]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        val neighbors: N = k._3.asInstanceOf[N]
        val mazeType: MazeType = k._4
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        // acc ++ Seq(Some(Cell(coords = coords, visited = visited, neighbors = neighbors, linked = linked)))
        val cell: C = Cell.instantiate[N, C](mazeType, coords, visited, neighbors, linked)
        acc ++ Seq(Some(cell))
      }
    }
    merged.filter(_.isDefined).map(_.get)
  }
  def link(cell1: C, cell2: C, bidi: Boolean)(implicit ct: ClassTag[C]): Seq[C] = bidi match {
    // case false => Seq(cell1.copy(linked = cell1.linked + cell2.coords))
    // case true => Seq(cell1.copy(linked = cell1.linked + cell2.coords), cell2.copy(linked = cell2.linked + cell1.coords))
    case false => Seq(Cell.setLinked[N, C](cell1, cell1.linked + cell2.coords))
    case true => Seq(Cell.setLinked[N, C](cell1, cell1.linked + cell2.coords), Cell.setLinked[N, C](cell2, cell2.linked + cell1.coords))
  }
  def link(cells: Seq[C], bidi: Boolean = true)(implicit ct: ClassTag[C]): Seq[C] = linkAll(cells, bidi, link)

  def link(cell1: C, cell2: C, grid: G)(implicit ct: ClassTag[C]): G = {
    if (cell1.neighbors.toSeq.contains(cell2.coords) && cell2.neighbors.toSeq.contains(cell1.coords)) {
      // val updated1: Cell = cell1.copy(linked = cell1.linked ++ Set(cell2.coords))
      // val updated2: Cell = cell2.copy(linked = cell2.linked ++ Set(cell1.coords))
      val updated1: C = Cell.setLinked[N, C](cell1, cell1.linked ++ Set(cell2.coords))
      val updated2: C = Cell.setLinked[N, C](cell2, cell2.linked ++ Set(cell1.coords))
      grid.set[G](updated1).set[G](updated2) 
    } else {
      grid
    }
  }
}

// import maze.classes.{ Coordinates, Neighbors, Cell, Grid }

// trait Linkage {

//   def visit(cell: Cell): Cell = cell.copy(visited = true)
  
//   def linked(cell1: Cell, cell2: Cell, bidi: Boolean = true): Boolean = bidi match {
//     case false => cell1.linked.contains(cell2.coords)
//     case true => cell1.linked.contains(cell2.coords) && cell2.linked.contains(cell1.coords) 
//   }
//   def linkAll(cells: Seq[Cell], bidi: Boolean, f: (Cell, Cell, Boolean) => Seq[Cell]): Seq[Cell] = {
//     val ungrouped: Seq[Cell] = (for ((c1, c2) <- cells zip cells.drop(1)) yield f(c1, c2, bidi)).flatten
//     val grouped = ungrouped.groupBy(c => (c.coords, c.visited, c.neighbors))
//     val merged: Seq[Option[Cell]] = grouped.foldLeft(Nil: Seq[Option[Cell]]) {
//       case (acc, (k, v)) => {
//         val coords: Coordinates = k._1
//         val visited: Boolean = k._2
//         val neighbors: Neighbors = k._3
//         val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
//         acc ++ Seq(Some(Cell(coords = coords, visited = visited, neighbors = neighbors, linked = linked)))
//       }
//     }
//     merged.filter(_.isDefined).map(_.get)
//   }
//   def link(cell1: Cell, cell2: Cell, bidi: Boolean): Seq[Cell] = bidi match {
//     case false => Seq(cell1.copy(linked = cell1.linked + cell2.coords))
//     case true => Seq(cell1.copy(linked = cell1.linked + cell2.coords), cell2.copy(linked = cell2.linked + cell1.coords))
//   }
//   def link(cells: Seq[Cell], bidi: Boolean = true): Seq[Cell] = linkAll(cells, bidi, link)

//   def link(cell1: Cell, cell2: Cell, grid: Grid): Grid = {
//     if (cell1.neighbors.toSeq.contains(cell2.coords) && cell2.neighbors.toSeq.contains(cell1.coords)) {
//       val updated1: Cell = cell1.copy(linked = cell1.linked ++ Set(cell2.coords))
//       val updated2: Cell = cell2.copy(linked = cell2.linked ++ Set(cell1.coords))
//       grid.set(updated1).set(updated2) 
//     } else {
//       grid
//     }
//   }
// }