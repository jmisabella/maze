package maze.classes

import maze.classes.Cell

case class Grid(
  rows: Int, 
  columns: Int, 
  cells: Array[Array[Cell]] = Array()) {

  def get(row: Int, column: Int): Cell = cells(row)(column)

  def links(cell: Cell): Seq[Cell] = (for (c <- cell.linked) yield cells(c.x)(c.y)).toSeq

  def linked(cell1: Cell, cell2: Cell): Boolean = cell1.isLinked(Some(cell2.coords))
  
  // def prepareGrid(): Grid = this.copy(cells = Array.ofDim[Cell](this.rows, this.columns))

  // def prepareCells(): Grid = { 
  //   this.copy(
  //     cells = (for (row <- 0 until rows) yield {
  //       // set cells' neighbors
  //       (for (col <- 0 until columns) yield {
  //         val cell = cells(row)(col)
  //         val north = cell.coords.x match {
  //           case 0 => None
  //           case _ => Some((cells(cell.coords.x - 1)(cell.coords.y)).coords)
  //         }
  //         val east = cell.coords.y match {
  //           case y if (y >= columns) => None
  //           case _ => Some((cells(cell.coords.x)(cell.coords.y + 1)).coords)
  //         }
  //         val south = cell.coords.x match {
  //           case x if (x >= rows) => None
  //           case _ => Some((cells(cell.coords.x + 1)(cell.coords.y)).coords)
  //         }
  //         val west = cell.coords.y match {
  //           case 0 => None
  //           case _ => Some((cells(cell.coords.x)(cell.coords.y - 1)).coords)
  //         }
  //         cell.copy(neighbors = Neighbors(north, east, south, west))
  //       }).toArray
  //     }).toArray
  //   )
  // }

}

