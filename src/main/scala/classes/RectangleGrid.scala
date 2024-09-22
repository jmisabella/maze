package maze.classes

import maze.behaviors.{ Cell, Grid }
import maze.classes.{ SquareCell, Coordinates, MazeType }
import maze.classes.MazeType._
import maze.classes.SquareDirection._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing
import scala.annotation.tailrec

case class RectangleGrid(
  height: Int, 
  width: Int, 
  cells: Array[Array[SquareCell]],
  seed: RNG,
  startCoords: Coordinates,
  goalCoords: Coordinates) extends Grid[SquareCell] {

  override val mazeType = Square
  
  override def asci(): String = {
    var output: String = "+" + "---+" * width + "\n"
    for (row <- cells) {
      var top: String = "|"
      var bottom: String = "+"
      for (cell <- row) {
        val body = cell.value
        val eastBoundary: String = cell.neighbors.get("east").isDefined match {
          case true if (cell.isLinked(East)) => " "
          case _ => "|"
        }
        top += body + eastBoundary
        val southBoundary: String = cell.neighbors.get("south").isDefined match {
          case true if (cell.isLinked(South)) => "   "
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

object RectangleGrid {
  def apply(height: Int, width: Int, start: Coordinates, goal: Coordinates): RectangleGrid = {
    val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))
    val empty: RectangleGrid = RectangleGrid(height, width, Array[Array[SquareCell]](), seed, start, goal).copy(cells = Array.ofDim[SquareCell](height, width))
    val grid: RectangleGrid = empty.copy(cells =
      (for (row <- 0 until empty.height) yield {
        (for (col <- 0 until empty.width) yield {
          SquareCell(col, row)
        }).toArray
      }).toArray
    )
    grid.copy(
      cells = (for (row <- 0 until grid.height) yield {
        var neighbors = Map[String, Coordinates]() 
        // set cells' neighbors
        (for (col <- 0 until grid.width) yield {
          val coordinates: Coordinates = Coordinates(col, row)
          val cell = grid.cells(row)(col)
          if (cell.coords.y != 0) {
            neighbors += ("north" -> grid.cells(cell.coords.y - 1)(cell.coords.x).coords)
          }
          if (cell.coords.x < grid.width - 1) {
            neighbors += ("east" -> (grid.cells(cell.coords.y)(cell.coords.x + 1)).coords)
          }
          if (cell.coords.y < grid.height - 1) {
            neighbors += ("south" -> (grid.cells(cell.coords.y + 1)(cell.coords.x)).coords)
          }
          if (cell.coords.x != 0) {
            neighbors += ("west" -> (grid.cells(cell.coords.y)(cell.coords.x - 1)).coords)
          }
          cell.copy(
            neighbors = neighbors,
            isStart = cell.coords == start,
            isGoal = cell.coords == goal)
        }).toArray
      }).toArray
    )
  }
}
