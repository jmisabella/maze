package maze.classes

import maze.behaviors.{ Cell, Grid }
import maze.classes.{ SquareCell, Coordinates, MazeType }
import maze.classes.MazeType._
import maze.classes.SquareDirection._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing
import scala.annotation.tailrec

case class SquareGrid(
  height: Int, 
  width: Int, 
  cells: Array[Array[SquareCell]],
  seed: RNG,
  startCoords: Coordinates,
  goalCoords: Coordinates) extends Grid[SquareCell] {

  override type NEIGHBORS = SquareNeighbors

  // retrieve row
  def row(y: Int): List[SquareCell] = cells(y).toList
  // retrieve column
  def column(x: Int): List[SquareCell] = (for (y <- 0 until height) yield cells(y)(x)).toList

  override val mazeType = Square

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

  override def asci(): String = {
    var output: String = "+" + "---+" * width + "\n"
    for (row <- cells) {
      var top: String = "|"
      var bottom: String = "+"
      for (cell <- row) {
        val body = cell.value
        val eastBoundary: String = cell.asInstanceOf[SquareCell].neighbors.east.isDefined match {
          case true if (cell.asInstanceOf[SquareCell].isLinked(East)) => " "
          case _ => "|"
        }
        top += body + eastBoundary
        val southBoundary: String = cell.asInstanceOf[SquareCell].neighbors.south.isDefined match {
          case true if (cell.asInstanceOf[SquareCell].isLinked(South)) => "   "
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

object SquareGrid {
  def apply(height: Int, width: Int, start: Coordinates, goal: Coordinates): SquareGrid = {
    val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))
    val empty: SquareGrid = SquareGrid(height, width, Array[Array[SquareCell]](), seed, start, goal).copy(cells = Array.ofDim[SquareCell](height, width))
    val grid: SquareGrid = empty.copy(cells =
      (for (row <- 0 until empty.height) yield {
        (for (col <- 0 until empty.width) yield {
          SquareCell(col, row)
        }).toArray
      }).toArray
    )
    grid.copy(
      cells = (for (row <- 0 until grid.height) yield {
        // set cells' neighbors
        (for (col <- 0 until grid.width) yield {
          val coordinates: Coordinates = Coordinates(col, row)
          val cell = grid.cells(row)(col)
          val north = cell.coords.y match {
            case 0 => None // nothing exists north
            case _ => Some((grid.cells(cell.coords.y - 1)(cell.coords.x)).coords)
          }
          val east = cell.coords.x match {
            case x if (x >= grid.width - 1) => None // nothing exists east
            case _ => Some((grid.cells(cell.coords.y)(cell.coords.x + 1)).coords)
          }
          val south = cell.coords.y match {
            case y if (y >= grid.height - 1) => None // nothing exists south
            case _ => Some((grid.cells(cell.coords.y + 1)(cell.coords.x)).coords)
          }
          val west = cell.coords.x match {
            case 0 => None // nothing exists west
            case _ => Some((grid.cells(cell.coords.y)(cell.coords.x - 1)).coords)
          }
          cell.copy(
            neighbors = SquareNeighbors(north, east, south, west),
            isStart = cell.coords == start,
            isGoal = cell.coords == goal)
        }).toArray
      }).toArray
    )
  }
}
