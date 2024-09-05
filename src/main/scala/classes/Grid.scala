package maze.classes

import maze.classes.{ Cell, Coordinates }
import maze.classes.Direction._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing
import scala.annotation.tailrec

case class Grid(
  rows: Int, 
  columns: Int, 
  cells: Array[Array[Cell]],
  seed: RNG,
  startCoords: Coordinates,
  goalCoords: Coordinates) {

  // retrieve cell residing at provided row and column coordinates
  def get(x: Int, y: Int): Cell = cells(y)(x)
  def get(coords: Coordinates): Cell = get(coords.x, coords.y)
  def get(cell: Cell): Cell = get(cell.coords)
  // retrieve row
  def row(y: Int): List[Cell] = cells(y).toList
  // retrieve column
  def column(x: Int): List[Cell] = (for (y <- 0 until rows) yield cells(y)(x)).toList

  def size(): Int = rows * columns

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

  def links(cell: Cell): Seq[Cell] = (for (c <- cell.linked) yield cells(c.y)(c.x)).toSeq

  def linked(cell1: Cell, cell2: Cell): Boolean = cell1.isLinked(cell2)

  // given a cell, returns its neighbor cells
  def neighbors(cell: Cell): Seq[Cell] = cell.neighbors.toSeq().map(c => get(c.x, c.y))
  def neighbors(coords: Coordinates): Seq[Cell] = get(coords).neighbors.toSeq().map(c => get(c.x, c.y))
  // given a cell, returns its unlinked neighbor cells
  def unlinkedNeighbors(cell: Cell): Seq[Cell] = cell.unlinkedNeighbors().map(c => get(c.x, c.y))
  def unlinkedNeighbors(coords: Coordinates): Seq[Cell] = get(coords).unlinkedNeighbors().map(c => get(c.x, c.y))
  // given a cell, returns its linked neighbor cells
  def linkedNeighbors(cell: Cell): Seq[Cell] = cell.linkedNeighbors().map(c => get(c.x, c.y))
  def linkedNeighbors(coords: Coordinates): Seq[Cell] = get(coords).linkedNeighbors().map(c => get(c.x, c.y))

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
    val grouped = flattened.groupBy(c => (c.coords, c.visited, c.neighbors, c.value, c.distance, c.onSolutionPath))
    val merged: Seq[Option[Cell]] = grouped.foldLeft(Nil: Seq[Option[Cell]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        val neighbors: Neighbors = k._3
        val value: String = k._4
        val distance: Int = k._5
        val onSolutionPath: Boolean = k._6
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        acc ++ Seq(Some(Cell(coords = coords, visited = visited, neighbors = neighbors, linked = linked, value = value, distance = distance, onSolutionPath = onSolutionPath)))
      }
    }
    val mergedCells: Seq[Cell] = merged.filter(_.isDefined).map(_.get)
    var remaining: List[Cell] = mergedCells.toList.sorted
    val empty: Grid = Grid(rows, columns, startCoords, goalCoords).copy(cells = Array.ofDim[Cell](rows, columns))
    empty.copy(cells =
      (for (row <- 0 until empty.rows) yield {
        (for (col <- 0 until empty.columns) yield {
          var cell = remaining.head
          remaining = remaining.tail
          val coordinates: Coordinates = Coordinates(col, row)
          cell.copy(isStart = coordinates == startCoords, isGoal = coordinates == goalCoords)
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
  def find[A <: Cell](p: Cell => Boolean): Option[Cell] = flatten().find(p)

  private def allConnectedCells(startCell: Cell): Seq[Cell] = {
    var connected: Seq[Cell] = Seq(startCell)
    var frontier: Seq[Cell] = Seq(startCell)
    while (!frontier.isEmpty) {
      var newFrontier: Seq[Cell] = Nil
      for (c <- frontier) {
        for (linked <- c.linked) {
          if (!connected.contains(get(linked))) {
            connected = connected ++ Seq(get(linked))
            newFrontier = newFrontier ++ Seq(cells(linked.y)(linked.x))
          }
        }
      }
      frontier = newFrontier
    }
    connected
  }
  def reachable(): Seq[Cell] = allConnectedCells(get(startCoords))
  def unreachable(): Seq[Cell] = flatten().diff(reachable())
  def isFullyConnected(): Boolean = allConnectedCells(get(startCoords)).size == size()
  def isPerfectMaze(): Boolean = {
    // each edge is counted twice, so divide by 2
    def countEdges(grid: Grid): Int = flatten().map(_.linked.toSeq.length).sum / 2
    // a maze is perfect if it's fully connected and is a tree (no cycles and exactly v-1 edges)
    isFullyConnected() && countEdges(this) == size() - 1
  }
  def linkOneUnreachable(): Grid = {
    var nextGrid = this 
    if (!nextGrid.isFullyConnected()) {
      var reachableCells: Seq[Cell] = nextGrid.reachable()
      var unreachableCells: Seq[Cell] = nextGrid.unreachable()
      for (unreached <- unreachableCells) {
        for (neighborCoords <- unreached.unlinkedNeighbors()) {
          var cell: Cell = unreached 
          var neighbor: Cell = get(neighborCoords)
          if (reachable.contains(neighbor)) {
            cell = cell.copy(linked = cell.linked ++ Set(neighbor.coords))
            neighbor = neighbor.copy(linked = neighbor.linked ++ Set(cell.coords))
            nextGrid = nextGrid.set(cell).set(neighbor)
            return nextGrid
          }
        } 
      }
    }
    nextGrid
  }
  
  def linkUnreachables(): Grid = {
    var nextGrid = this
    while (!nextGrid.isFullyConnected()) {
      nextGrid = nextGrid.linkOneUnreachable()
    }
    nextGrid
  }
  
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

  def asci(): String = {
    var output: String = "+" + "---+" * columns + "\n"
    for (row <- cells) {
      var top: String = "|"
      var bottom: String = "+"
      for (cell <- row) {
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
  def apply(rows: Int, columns: Int, start: Coordinates, goal: Coordinates): Grid = {
    val seed: RNG = RNG.RandomSeed(Random.nextInt(rows * columns + 1))
    val empty: Grid = Grid(rows, columns, Array[Array[Cell]](), seed, start, goal).copy(cells = Array.ofDim[Cell](rows, columns))
    val grid: Grid = empty.copy(cells =
      (for (row <- 0 until empty.rows) yield {
        (for (col <- 0 until empty.columns) yield {
          Cell(col, row)
        }).toArray
      }).toArray
    )
    grid.copy(
      cells = (for (row <- 0 until grid.rows) yield {
        // set cells' neighbors
        (for (col <- 0 until grid.columns) yield {
          val coordinates: Coordinates = Coordinates(col, row)
          val cell = grid.cells(row)(col)
          val north = cell.coords.y match {
            case 0 => None // nothing exists north
            case _ => Some((grid.cells(cell.coords.y - 1)(cell.coords.x)).coords)
          }
          val east = cell.coords.x match {
            case x if (x >= grid.columns - 1) => None // nothing exists east
            case _ => Some((grid.cells(cell.coords.y)(cell.coords.x + 1)).coords)
          }
          val south = cell.coords.y match {
            case y if (y >= grid.rows - 1) => None // nothing exists south
            case _ => Some((grid.cells(cell.coords.y + 1)(cell.coords.x)).coords)
          }
          val west = cell.coords.x match {
            case 0 => None // nothing exists west
            case _ => Some((grid.cells(cell.coords.y)(cell.coords.x - 1)).coords)
          }
          cell.copy(
            neighbors = Neighbors(north, east, south, west),
            isStart = cell.coords == start,
            isGoal = cell.coords == goal)
        }).toArray
      }).toArray
    )
  }
}
