package maze.classes

import maze.classes.Cell
import maze.classes.Direction
import maze.classes.Direction._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing
import scala.annotation.tailrec
import java.net.CookieStore

case class Grid(
  rows: Int, 
  columns: Int, 
  cells: Array[Array[Cell]],
  seed: RNG) {

  // retrieve cell residing at provided row and column coordinates
  def get(x: Int, y: Int): Cell = cells(x)(y)
  def get(coords: Coordinates): Cell = cells(coords.x)(coords.y)
  // retrieve row
  def row(x: Int): List[Cell] = cells(x).toList
  // retrieve column
  def column(y: Int): List[Cell] = (for (i <- 0 until rows) yield cells(i)(y)).toList

  // given a cell (which tracks its own x,y coordinates) updates grid's cell at those coordinates
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

  def linked(cell1: Cell, cell2: Cell): Boolean = cell1.isLinked(cell2)

  def padRight(s: String, c: Char, n: Int): String = s.padTo(n, c).mkString
  def padLeft(s: String, c: Char, n: Int): String = n match {
    case 0 => s
    case x if (x < 0) => s
    case _ => padRight(s, c, n).split(s).tail.mkString + s
  }
  // evenly pad left and right; left has 1 extra padding in case of an odd length 
  def pad(s:String, c: Char, n:Int): String = {
    val left = (n - s.length) / 2
    val right = n - left - s.length 
    c.toString * left + s + c.toString * right
  }

  // given list of Cells, converts list to grid (array of arrays of cells)
  // prerequisite: provided list's length equals our grid's rows multiplied by columns
  def unflatten(flattened: Seq[Cell]): Grid = {
    val grouped = flattened.groupBy(c => (c.coords, c.visited, c.neighbors, c.value))
    val merged: Seq[Option[Cell]] = grouped.foldLeft(Nil: Seq[Option[Cell]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        val neighbors: Neighbors = k._3
        val value: String = k._4
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        acc ++ Seq(Some(Cell(coords = coords, visited = visited, neighbors = neighbors, linked = linked, value = value)))
      }
    }
    val mergedCells: Seq[Cell] = merged.filter(_.isDefined).map(_.get)
    var remaining: List[Cell] = mergedCells.toList.sorted
    val empty: Grid = Grid(rows, columns).copy(cells = Array.ofDim[Cell](rows, columns))
    empty.copy(cells =
      (for (row <- 0 until empty.rows) yield {
        (for (col <- 0 until empty.columns) yield {
          val cell = remaining.head
          remaining = remaining.tail
          cell
        }).toArray
      }).toArray
    )
  }
  def randomInt(upperBoundary: Int): (Int, RNG) = seed.boundedPositiveInt(upperBoundary)
  def randomBoolean(): (Boolean, RNG) = seed.nextBoolean

  def reverseRows(): Array[Array[Cell]] = cells.toList.reverse.toArray

  // common monad and other useful functions
  def flatten(): List[Cell] = cells.flatten.toList
  def foreach(block: Cell => Unit): Unit = cells.foreach(row => row.foreach(block))
  def count[A <: Cell](p: Cell => Boolean): Int = flatten().count(p)
  def map(f: Cell => Cell): Grid = unflatten(flatten().map(f))
  // TODO: I think we need to re-work filtering in order to preserve original grid size! 
  // TODO: If cells are filtered out then grid should use null to represent missing cells.
  def withFilter(p: Cell => Boolean): Grid = unflatten(flatten().filter(p))
  def filter(p: Cell => Boolean): Grid = withFilter(p)
  def contains[A <: Cell](c: A): Boolean = flatten().contains(c.asInstanceOf[Cell])
  def contains[A <: Cell](cs: Seq[A]): Boolean = flatten().foldLeft(false)((acc, c) => contains(c)) 

  override def toString(): String = {
    var output: String = "{\"rows\":["
    var currRow: String = ""
    for (row <- cells) {
      if (currRow.length() > 0) {
        output += ","
      } 
      currRow = row.mkString("[", ",", "]")
      output += currRow
    }
    output += "]}"
    output
  }

  // override def toString(): String = {
  def asci(): String = {
    var output: String = "+" + "---+" * columns + "\n"
    for (row <- cells) {
      var top: String = "|"
      var bottom: String = "+"
      for (cell <- row) {
        // val body = " X "
        val body = cell.value
        val eastBoundary: String = cell.neighbors.east match {
          case Some(east) if (cell.isLinked(east)) => " "
          case _ => "|"
        }
        top += body + eastBoundary
        val southBoundary: String = cell.neighbors.south match {
          case Some(south) if (cell.isLinked(south)) => "   "
          case _ => "---"
        }
        val corner: String= "+"
        bottom += southBoundary + corner
      }
      output += top + "\n"
      output += bottom + "\n"
    }
    output 
  }

}

object Grid {
  def apply(rows: Int, columns: Int): Grid = {
    val seed: RNG = RNG.RandomSeed(Random.nextInt(rows * columns + 1))
    val empty: Grid = Grid(rows, columns, Array[Array[Cell]](), seed).copy(cells = Array.ofDim[Cell](rows, columns))
    val grid: Grid = empty.copy(cells =
      (for (row <- 0 until empty.rows) yield {
        (for (col <- 0 until empty.columns) yield {
          Cell(row, col)
        }).toArray
      }).toArray
    )
    grid.copy(
      cells = (for (row <- 0 until grid.rows) yield {
        // set cells' neighbors
        (for (col <- 0 until grid.columns) yield {
          val cell = grid.cells(row)(col)
          val north = cell.coords.x match {
            case 0 => None // nothing exists north
            case _ => Some((grid.cells(cell.coords.x - 1)(cell.coords.y)).coords)
          }
          val east = cell.coords.y match {
            case y if (y >= grid.columns - 1) => None // nothing exists east
            case _ => Some((grid.cells(cell.coords.x)(cell.coords.y + 1)).coords)
          }
          val south = cell.coords.x match {
            case x if (x >= grid.rows - 1) => None // nothing exists south
            case _ => Some((grid.cells(cell.coords.x + 1)(cell.coords.y)).coords)
          }
          val west = cell.coords.y match {
            case 0 => None // nothing exists west
            case _ => Some((grid.cells(cell.coords.x)(cell.coords.y - 1)).coords)
          }
          val northeast = (cell.coords.x, cell.coords.y) match {
            case (0, _) => None // nothing exists north
            case (_, y) if (y >= grid.columns - 1) => None // nothing exists east
            case (x, y) => Some((grid.cells(cell.coords.x - 1)(cell.coords.y + 1)).coords)
          }
          val southeast = (cell.coords.x, cell.coords.y) match {
             case (x, _) if (x >= grid.rows - 1) => None // nothing exists south
             case (_, y) if (y >= grid.columns - 1) => None // nothing exists east
             case (x, y) => Some((grid.cells(cell.coords.x + 1)(cell.coords.y + 1)).coords)
          }
          val southwest = (cell.coords.x, cell.coords.y) match {
            case (x, _) if (x >= grid.rows - 1) => None // nothing exists south
            case (_, 0) => None // nothing exists west
            case (x, y) => Some((grid.cells(cell.coords.x + 1)(cell.coords.y - 1)).coords)
          }
          val northwest = (cell.coords.x, cell.coords.y) match {
            case (0, _) => None // nothing exists north
            case (_, 0) => None // nothing exists west
            case (x, y) => Some((grid.cells(cell.coords.x - 1)(cell.coords.y - 1)).coords)
          }
          cell.copy(neighbors = Neighbors(north, east, south, west, northeast, southeast, southwest, northwest))
        }).toArray
      }).toArray
    )
  }
}
