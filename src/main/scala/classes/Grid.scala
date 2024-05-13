package maze.classes

import maze.classes.Cell
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing

case class Grid(
  rows: Int, 
  columns: Int, 
  cells: Array[Array[Cell]],
  seed: RNG) {

  // retrieve cell residing at provided row and column coordinates
  def get(x: Int)(y: Int): Cell = cells(x)(y)
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

  // given list of Cells, converts list to grid (array of arrays of cells)
  // prerequisite: provided list's length equals our grid's rows multiplied by columns
  // TODO: this method does not work correctly, seems to be losing the final row when using BinaryTree to construct the maze! 
  // TODO: I believe we need to rework this method to group by coordinates in order to merge all linked coordinates together! 
  def unflatten(flattened: Seq[Cell]): Grid = {
    // require(
    //   flattened.length == rows * columns, 
    //   s"When unflattening Grid, flattened cell count [${flattened.length}] does not equal $rows rows multiplied by $columns columns")
    val grouped = flattened.groupBy(c => (c.coords, c.visited, c.neighbors))
    val merged: Seq[Option[Cell]] = grouped.foldLeft(Nil: Seq[Option[Cell]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        val neighbors: Neighbors = k._3
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        acc ++ Seq(Some(Cell(coords = coords, visited = visited, neighbors = neighbors, linked = linked)))
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
    var output: String = "+" + "---+" * columns + "\n"
    for (row <- cells) {
      var top: String = "|"
      var bottom: String = "+"
      for (cell <- row) {
        val body = "   "
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

  // override def toString(): String = {
  //   val horizontalLineThick: String = "\u2501"
  //   val verticalLineThick: String = "\u2503"
  //   val fourWayJunctureThick: String = "\u254B"
  //   val threeWayJunctureNorthwardThick: String = "\u253B"
  //   val threeWayJunctureEastwardThick: String = "\u2523"
  //   val threeWayJunctureSouthwardThick: String = "\u2533"
  //   val threeWayJunctureWestwardThick: String = "\u252B" 
  //   val northWestCornerThick: String = "\u250F"
  //   val northEastCornerThick: String = "\u2513"
  //   val southEastCornerThick: String = "\u251B"
  //   val southWestCornerThick: String = "\u2517"
  //   // var output: String = "+" + "---+" * columns + "\n"
  //   // var output: String = "+" + horizontalLineThick * columns + "\n"
  //   var output: String = "+" + horizontalLineThick * 4 * columns + "\n"
  //   for (row <- cells) {
  //     // var top: String = "|"
  //     var top: String = verticalLineThick 
  //     var bottom: String = "+"
  //     for (cell <- row) {
  //       val body = "   "
  //       val eastBoundary: String = cell.neighbors.east match {
  //         case Some(east) if (cell.isLinked(east)) => " "
  //         // case _ => "|"
  //         case _ => verticalLineThick
  //       }
  //       top += body + eastBoundary
  //       val southBoundary: String = cell.neighbors.south match {
  //         case Some(south) if (cell.isLinked(south)) => "   "
  //         // case _ => "---"
  //         case _ => horizontalLineThick * 3
  //       }
  //       // val corner: String= "+"
  //       // bottom += southBoundary + corner
  //       val linkEnding: String = (cell.neighbors.north, cell.neighbors.east, cell.neighbors.south, cell.neighbors.west) match {
  //         // no boundaries 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(north) && cell.isLinked(east) && cell.isLinked(south) && cell.isLinked(west)) => fourWayJunctureThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(north) && cell.isLinked(east) && cell.isLinked(south)) => threeWayJunctureEastwardThick 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(east) && cell.isLinked(south) && cell.isLinked(west)) => threeWayJunctureSouthwardThick 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(north) && cell.isLinked(south) && cell.isLinked(west)) => threeWayJunctureWestwardThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(north) && cell.isLinked(east) && cell.isLinked(west)) => threeWayJunctureNorthwardThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(east) && cell.isLinked(south)) => northWestCornerThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(south) && cell.isLinked(west)) => northEastCornerThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(north) && cell.isLinked(west)) => southEastCornerThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(north) && cell.isLinked(east)) => southWestCornerThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(north)) => "A" //"0" // not sure which character to use for vertical line endings 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(east)) => "B" //horizontalLineThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(south)) => "C" //"0" // not sure which character to use for vertical line endings 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(west)) => "D" //horizontalLineThick
  //         // northern boundaries
  //         case (None, Some(east), Some(south), Some(west)) if (cell.isLinked(east) && cell.isLinked(south) && cell.isLinked(west)) => threeWayJunctureSouthwardThick
  //         case (None, Some(east), Some(south), _) if (cell.isLinked(east) && cell.isLinked(south)) => northWestCornerThick
  //         case (None, _, Some(south), Some(west)) if (cell.isLinked(south) && cell.isLinked(west)) => northEastCornerThick
  //         case (None, Some(east), _, Some(west)) if (cell.isLinked(east) && cell.isLinked(west)) => horizontalLineThick 
  //         // eastern boundaries
  //         case (Some(north), None, Some(south), Some(west)) if (cell.isLinked(north) && cell.isLinked(south) && cell.isLinked(west)) => threeWayJunctureWestwardThick
  //         case (Some(north), None, _, Some(west)) if (cell.isLinked(north) && cell.isLinked(west)) => southEastCornerThick
  //         case (_, None, Some(south), Some(west)) if (cell.isLinked(south) && cell.isLinked(west)) => northEastCornerThick
  //         case (Some(north), None, Some(south), _) if (cell.isLinked(north) && cell.isLinked(south)) => verticalLineThick
  //         // southern boundaries
  //         case (Some(north), Some(east), None, Some(west)) if (cell.isLinked(north) && cell.isLinked(east) && cell.isLinked(west)) => threeWayJunctureNorthwardThick
  //         case (Some(north), Some(east), None, _) if (cell.isLinked(north) && cell.isLinked(east)) => southWestCornerThick
  //         case (Some(north), _, None, Some(west)) if (cell.isLinked(north) && cell.isLinked(west)) => southEastCornerThick
  //         case (_, Some(east), None, Some(west)) if (cell.isLinked(east) && cell.isLinked(west)) => horizontalLineThick
  //         // western boundaries
  //         case (Some(north), Some(east), Some(south), None) if (cell.isLinked(north) && cell.isLinked(east) && cell.isLinked(south)) => threeWayJunctureEastwardThick
  //         case (Some(north), Some(east), _, None) if (cell.isLinked(north) && cell.isLinked(east)) => southWestCornerThick
  //         case (_, Some(east), Some(south), None) if (cell.isLinked(south) && cell.isLinked(east)) => northWestCornerThick
  //         case (Some(north), _, Some(south), None) if (cell.isLinked(north) && cell.isLinked(south)) => verticalLineThick
  //         case _ => "o"
  //       }
  //       bottom += southBoundary + linkEnding
  //     }
  //     output += top + "\n"
  //     output += bottom + "\n"
  //   }
  //   output 
  // }

  // override def toString(): String = {
  //   val horizontalLineThick: String = "\u2501"
  //   val verticalLineThick: String = "\u2503"
  //   val fourWayJunctureThick: String = "\u254B"
  //   val threeWayJunctureNorthwardThick: String = "\u253B"
  //   val threeWayJunctureEastwardThick: String = "\u2523"
  //   val threeWayJunctureSouthwardThick: String = "\u2533"
  //   val threeWayJunctureWestwardThick: String = "\u252B" 
  //   val northWestCornerThick: String = "\u250F"
  //   val northEastCornerThick: String = "\u2513"
  //   val southEastCornerThick: String = "\u251B"
  //   val southWestCornerThick: String = "\u2517"

  //   val topWall: String = northWestCornerThick + (horizontalLineThick * 4 * (columns - 1)) + (horizontalLineThick * 3) + northEastCornerThick + "\n" 
  //   val bottomWall: String = southWestCornerThick + (horizontalLineThick * 4 * (columns - 1)) + (horizontalLineThick * 3) + southEastCornerThick + "\n" 

  //   var output: String = ""
  //   output += topWall 
  //   for (row <- cells) {
  //     // var top: String = "|"
  //     var top: String = verticalLineThick 
  //     // var bottom: String = "+"
  //     var bottom: String = "" 
  //     for (cell <- row) {
  //       if (bottom.length() == 0) {
  //         cell.neighbors.south match {
  //           case Some(south) => bottom += verticalLineThick
  //           case None => bottom += southWestCornerThick
  //         }
  //       }
  //       val body = "   "
  //       val eastward: String = cell.neighbors.east match {
  //         case Some(east) if (cell.isLinked(east)) => " "
  //         case _ => verticalLineThick
  //       }
  //       top += body + eastward
  //       val southward: String = cell.neighbors.south match {
  //         case Some(south) if (cell.isLinked(south)) => " " * 3
  //         case _ => horizontalLineThick * 3
  //       }
  //       // bottom += southBoundary + corner
  //       val linkEnding: String = (cell.neighbors.north, cell.neighbors.east, cell.neighbors.south, cell.neighbors.west) match {
  //         // no boundaries 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(east) && !cell.isLinked(south) && !cell.isLinked(west)) => fourWayJunctureThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(east) && !cell.isLinked(south)) => threeWayJunctureEastwardThick 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(east) && !cell.isLinked(south) && !cell.isLinked(west)) => threeWayJunctureSouthwardThick 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(south) && !cell.isLinked(west)) => threeWayJunctureWestwardThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(east) && !cell.isLinked(west)) => threeWayJunctureNorthwardThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(east) && !cell.isLinked(south)) => northEastCornerThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(south) && !cell.isLinked(west)) => northWestCornerThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(west)) => southWestCornerThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(east)) => southEastCornerThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(south)) => verticalLineThick //"0" // not sure which character to use for vertical line endings 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(north)) => "A" //"0" // not sure which character to use for vertical line endings 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(east)) => "B" //horizontalLineThick
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(south)) => "C" //"0" // not sure which character to use for vertical line endings 
  //         case (Some(north), Some(east), Some(south), Some(west)) if (cell.isLinked(west)) => "D" //horizontalLineThick
  //         // northern boundaries
  //         // case (None, Some(east), Some(south), Some(west)) if (!cell.isLinked(east) && !cell.isLinked(south) && !cell.isLinked(west)) => threeWayJunctureSouthwardThick
  //         case (None, Some(east), Some(south), Some(west)) if (!cell.isLinked(east) && !cell.isLinked(south) && !cell.isLinked(west)) => threeWayJunctureNorthwardThick
  //         case (None, Some(east), Some(south), _) if (!cell.isLinked(east) && !cell.isLinked(south)) => northWestCornerThick
  //         case (None, _, Some(south), Some(west)) if (!cell.isLinked(south) && !cell.isLinked(west)) => northEastCornerThick
  //         case (None, Some(east), _, Some(west)) if (!cell.isLinked(east) && !cell.isLinked(west)) => horizontalLineThick 
  //         // eastern boundaries
  //         // case (Some(north), None, Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(south) && !cell.isLinked(west)) => threeWayJunctureWestwardThick
  //         case (Some(north), None, Some(south), Some(west)) if (!cell.isLinked(north) && !cell.isLinked(south) && !cell.isLinked(west)) => threeWayJunctureEastwardThick
  //         case (Some(north), None, _, Some(west)) if (!cell.isLinked(north) && !cell.isLinked(west)) => southEastCornerThick
  //         case (_, None, Some(south), Some(west)) if (!cell.isLinked(south) && !cell.isLinked(west)) => northEastCornerThick
  //         case (Some(north), None, Some(south), _) if (!cell.isLinked(north) && !cell.isLinked(south)) => verticalLineThick
  //         // southern boundaries
  //         case (Some(north), Some(east), None, Some(west)) if (!cell.isLinked(north) && !cell.isLinked(east) && !cell.isLinked(west)) => threeWayJunctureSouthwardThick
  //         case (Some(north), Some(east), None, _) if (!cell.isLinked(north) && !cell.isLinked(east)) => southWestCornerThick
  //         case (Some(north), _, None, Some(west)) if (!cell.isLinked(north) && !cell.isLinked(west)) => southEastCornerThick
  //         case (_, Some(east), None, Some(west)) if (!cell.isLinked(east) && !cell.isLinked(west)) => horizontalLineThick
  //         // western boundaries
  //         case (Some(north), Some(east), Some(south), None) if (!cell.isLinked(north) && !cell.isLinked(east) && !cell.isLinked(south)) => threeWayJunctureWestwardThick
  //         case (Some(north), Some(east), _, None) if (!cell.isLinked(north) && !cell.isLinked(east)) => southWestCornerThick
  //         case (_, Some(east), Some(south), None) if (!cell.isLinked(south) && !cell.isLinked(east)) => northWestCornerThick
  //         case (Some(north), _, Some(south), None) if (!cell.isLinked(north) && !cell.isLinked(south)) => verticalLineThick
  //         ///////// 
  //         case (None, Some(east), Some(south), None) if (!cell.isLinked(east) && !cell.isLinked(south)) => horizontalLineThick 
  //         case (None, Some(east), Some(south), None) if (!cell.isLinked(east) && !cell.isLinked(south)) => verticalLineThick 
  //         case _ => "o"
  //       }
  //       bottom += southward + linkEnding
  //     }
  //     output += top + "\n"
  //     output += bottom + "\n"
  //   }
  //   output 
  // }

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
