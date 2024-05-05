package maze.classes

import maze.classes.Cell

case class Grid(
  rows: Int, 
  columns: Int, 
  cells: Array[Array[Cell]] = Array()) {

  def get(row: Int)(column: Int): Cell = cells(row)(column)

  def set(cell: Cell): Grid = {
    this.copy(cells = (for (row <- this.cells) yield {
      (for (c <- row) yield { 
        c.coords match {
          case cell.coords => cell
          case _ => c
        }
      }).toArray
    }).toArray)
  }
  
  def links(cell: Cell): Seq[Cell] = (for (c <- cell.linked) yield cells(c.x)(c.y)).toSeq

  def linked(cell1: Cell, cell2: Cell): Boolean = cell1.isLinked(Some(cell2.coords))

  def foreach(block: Cell => Unit): Unit = cells.foreach(row => row.foreach(block))
  def count[A <: Cell](p: Cell => Boolean): Int = cells.flatten.count(p)
  // def map(f: Cell => Cell): Grid = Deck(cards.map(f), seed)
  // def withFilter(p: Card => Boolean): Deck = Deck(cards.filter(p), seed)
  // def filter(p: Card => Boolean): Deck = withFilter(p)
  // def contains[A <: Card](c: A): Boolean = cards.contains(c.asInstanceOf[Card])
  // def contains[A <: Card](cs: Seq[A]): Boolean = cs.foldLeft(false)((acc, c) => contains(c)) 
}

