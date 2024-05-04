package maze.classes

import maze.classes.Cell

case class Grid(
  rows: Int, 
  columns: Int, 
  cells: Array[Array[Cell]] = Array()) {

  def links(cell: Cell): Seq[Cell] = (for (c <- cell.linked) yield cells(c.x)(c.y)).toSeq
  def linked(cell1: Cell, cell2: Cell): Boolean = cell1.isLinked(Some(cell2.coordinates))
  
  def prepareGrid(): Grid = this.copy(cells = Array.ofDim[Cell](this.rows, this.columns))

  // def prepareCells(): Grid = this.copy(cells = ) 

}

