package maze.behaviors

import maze.classes.{ Cell, Coordinates, Neighbors, Grid }

trait GridPreparation {

  def initialize(rows: Int, columns: Int): Grid = prepareCells(prepareGrid(rows, columns))

  def prepareGrid(rows: Int, columns: Int): Grid = {
    val empty = Grid(rows, columns).copy(cells = Array.ofDim[Cell](rows, columns))
    empty.copy(cells =
      (for (row <- 0 until empty.rows) yield {
        (for (col <- 0 until empty.columns) yield {
          Cell(row, col)
        }).toArray
      }).toArray
    )
  }

  def prepareCells(grid: Grid): Grid = { 
    grid.copy(
      cells = (for (row <- 0 until grid.rows) yield {
        // set cells' neighbors
        (for (col <- 0 until grid.columns) yield {
          val cell = grid.cells(row)(col)
          val north = cell.coords.x match {
            case 0 => None
            case _ => Some((grid.cells(cell.coords.x - 1)(cell.coords.y)).coords)
          }
          val east = cell.coords.y match {
            case y if (y >= grid.columns - 1) => None
            case _ => Some((grid.cells(cell.coords.x)(cell.coords.y + 1)).coords)
          }
          val south = cell.coords.x match {
            case x if (x >= grid.rows - 1) => None
            case _ => Some((grid.cells(cell.coords.x + 1)(cell.coords.y)).coords)
          }
          val west = cell.coords.y match {
            case 0 => None
            case _ => Some((grid.cells(cell.coords.x)(cell.coords.y - 1)).coords)
          }
          cell.copy(neighbors = Neighbors(north, east, south, west))
        }).toArray
      }).toArray
    )
  }

}