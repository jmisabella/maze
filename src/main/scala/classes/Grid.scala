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

  def reachable(startX: Int, startY: Int, goalX: Int, goalY: Int): Boolean = {
    def linked(grid: Grid, coords: Coordinates): Seq[Coordinates] = {
      var linkedCoords: Seq[Coordinates] = Nil
      var frontier: Seq[Coordinates] = Seq(coords)
      while (!frontier.isEmpty) {
        var newFrontier: Seq[Coordinates] = Nil
        for (c <- frontier.map(c => grid.get(c.x)(c.y))) {
          for (linked <- c.linked) {
            if (!linkedCoords.contains(linked)) {
              linkedCoords = linkedCoords ++ Seq(linked)
              // distances = distances + (linked -> (distances.get(c.coords).getOrElse(-99999999) + 1))
              newFrontier = newFrontier ++ Seq(linked)
            }
          }
        }
        frontier = newFrontier
      }
      linkedCoords.distinct
    }
    val startLinkedCoordinates = linked(this, Coordinates(startX, startY))
    val endLinkedCoordinates = linked(this, Coordinates(goalX, goalY))
    !startLinkedCoordinates.intersect(endLinkedCoordinates).isEmpty
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

  // def distances(cell: Cell): Map[Coordinates, Int] = {
  //   var distances: Map[Coordinates, Int] = Map(cell.coords -> 0)
  //   var frontier: Seq[Cell] = Seq(cell)
  //   while (!frontier.isEmpty) {
  //     var newFrontier: Seq[Cell] = Nil
  //     for (c <- frontier) {
  //       for (linked <- c.linked) {
  //         if (!distances.keySet.contains(linked)) {
  //           distances = distances + (linked -> (distances.get(c.coords).getOrElse(0) + 1))
  //           newFrontier = newFrontier ++ Seq(this.get(linked))
  //         }
  //       }
  //     }
  //     frontier = newFrontier
  //   }
  //   distances
  // }
  // def showDistances(x: Int, y: Int): Grid = {
  //   val dist: Map[Coordinates, Int] = distances(x, y)
  //   val withDinstances: Seq[Cell] = cells.flatten.map(c => c.copy(value = pad(dist.get(c.coords).getOrElse(" ").toString(), ' ', 3)))
  //   unflatten(withDinstances)
  // }
  // def showDistances(coords: Coordinates): Grid = showDistances(coords.x, coords.y)

  // // def distances(cell: Cell): Map[Coordinates, Int] = {
  // //   var distances: Map[Coordinates, Int] = Map(cell.coords -> 0)
  // //   var frontier: Seq[Cell] = Seq(cell)
  // //   while (!frontier.isEmpty) {
  // //     var newFrontier: Seq[Cell] = Nil
  // //     for (c <- frontier) {
  // //       for (linked <- c.linked) {
  // //         if (distances.keySet.contains(linked)) {
  // //           distances = distances + (linked -> (distances.get(c.coords).getOrElse(0) + 1))
  // //           newFrontier = newFrontier ++ Seq(this.get(linked))
  // //         }
  // //       }
  // //     }
  // //     frontier = newFrontier
  // //   }
  // //   distances
  // // }
  // def distances(x: Int, y: Int): Map[Coordinates, Int] = this.distances(this.get(x)(y))
  // def distances(coords: Coordinates): Map[Coordinates, Int] = this.distances(this.get(coords))

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

  // override def toString(): String = {
  //   val horizontalLine: String = "\u2501"
  //   val verticalLine: String = "\u2503"
  //   val fourWayJuncture: String = "\u254B"
  //   val threeWayJunctureNorthward: String = "\u253B"
  //   val threeWayJunctureEastward: String = "\u2523"
  //   val threeWayJunctureSouthward: String = "\u2533"
  //   val threeWayJunctureWestward: String = "\u252B"
  //   val upperLeftCorner: String = "\u250F"
  //   val upperRightCorner: String = "\u2513"
  //   val bottomRightCorner: String = "\u251B"
  //   val bottomLeftCorner: String = "\u2517"
  //   var acc: String = "" // all accumulated text
  //   for (row <- cells) {
  //     // represents accumulated characters making up horizontal portion (top walls) of the cells in this row
  //     var topAcc: String = "" 
  //     // represents accumulated characters making up vertical portion (left walls) of the cells in this row 
  //     var middleAcc: String = ""
  //     // represents only the very bottom horizontal of the grid 
  //     var bottomAcc: String = ""
  //     for (cell <- row) {
  //       // determine all neighboring cells
  //       val north: Option[Cell] = cell.neighbors.north match {
  //         case None => None
  //         case Some(c) => Some(this.get(c))
  //       }
  //       val east: Option[Cell] = cell.neighbors.east match {
  //         case None => None
  //         case Some(c) => Some(this.get(c))
  //       }
  //       val south: Option[Cell] = cell.neighbors.south match {
  //         case None => None
  //         case Some(c) => Some(this.get(c))
  //       }
  //       val west: Option[Cell] = cell.neighbors.west match {
  //         case None => None
  //         case Some(c) => Some(this.get(c))
  //       }
  //       val northeast: Option[Cell] = cell.neighbors.northeast match {
  //         case None => None
  //         case Some(c) => Some(this.get(c))
  //       }
  //       val southeast: Option[Cell] = cell.neighbors.southeast match {
  //         case None => None
  //         case Some(c) => Some(this.get(c))
  //       }
  //       val southwest: Option[Cell] = cell.neighbors.southwest match {
  //         case None => None
  //         case Some(c) => Some(this.get(c))
  //       }
  //       val northwest: Option[Cell] = cell.neighbors.northwest match {
  //         case None => None
  //         case Some(c) => Some(this.get(c))
  //       }
  //       val isNortheastCorner: Boolean = !north.isDefined && !east.isDefined && south.isDefined && west.isDefined && southwest.isDefined
  //       val isSoutheastCorner: Boolean = north.isDefined && !east.isDefined && !south.isDefined && west.isDefined && northwest.isDefined
  //       val isSouthwestCorner: Boolean = north.isDefined && east.isDefined && !south.isDefined && !west.isDefined && northeast.isDefined
  //       val isNorthwestCorner: Boolean = !north.isDefined && east.isDefined && south.isDefined && !west.isDefined && southeast.isDefined
  //       val isNorthWall: Boolean = !north.isDefined && east.isDefined && south.isDefined && west.isDefined
  //       val isEastWall: Boolean = north.isDefined && !east.isDefined && south.isDefined && west.isDefined
  //       val isSouthWall: Boolean = north.isDefined && east.isDefined && !south.isDefined && west.isDefined
  //       val isWestWall: Boolean = north.isDefined && east.isDefined && south.isDefined && !west.isDefined
  //       val cellWidth: Int = 3
  //       val body = " " * cellWidth // "body" represents the empty space inside the cell
  //       // only draw the top wall when we are in the first row
  //       val cellTopWall: String = (isNorthwestCorner, isNorthWall, isNortheastCorner) match {
  //         case (true, _, _) => cell.isLinked(East) match {
  //           // case true => upperLeftCorner + horizontalLine * cellWidth + horizontalLine // linked to east
  //           case true => upperLeftCorner + horizontalLine * cellWidth // linked to east
  //           case false => upperLeftCorner + horizontalLine + threeWayJunctureSouthward // 3-way juncture since not linked east
  //         }
  //         case (_, true, _) => cell.isLinked(East) match {
  //           case true => horizontalLine * cellWidth + horizontalLine
  //           case false => horizontalLine * cellWidth + threeWayJunctureSouthward
  //         }
  //         case (_, _, true) => cell.isLinked(West) match {
  //           case true => threeWayJunctureWestward + horizontalLine * cellWidth + upperRightCorner
  //           case false => horizontalLine * cellWidth + upperRightCorner
  //         }
  //         case (false, false, false) => "" // if not on northern boundary then don't draw anything for top line 
  //       }
  //       // only draw left wall when we are in the first column
  //       val cellLeftWall: String = (isNorthwestCorner, isWestWall, isSouthwestCorner) match {
  //         case (true, _, _) => verticalLine
  //         case (_, true, _) => verticalLine
  //         case (_, _, true) => verticalLine
  //         case (false, false, false) => "" // if not on western boundary then don't draw anything for left wall
  //       }
  //       // always draw the bottom wall
  //       val cellBottomWall: String = (isSoutheastCorner, isSouthWall, isSoutheastCorner) match {
  //         // southeast corner
  //         case (true, _, _) => horizontalLine * cellWidth + bottomRightCorner
  //         // south wall
  //         case (_, true, _) => (cell.isLinked(West), cell.isLinked(East)) match {
  //           case (true, true) => horizontalLine + horizontalLine * cellWidth + horizontalLine
  //           case (true, false) => horizontalLine + horizontalLine * cellWidth + threeWayJunctureNorthward
  //           case (false, true) => threeWayJunctureNorthward + horizontalLine * cellWidth + horizontalLine
  //           case (false, false) => threeWayJunctureNorthward + horizontalLine * cellWidth + threeWayJunctureNorthward
  //         }
  //         // southwest corner
  //         case (_, _, true) => bottomLeftCorner + horizontalLine * cellWidth
  //         // middle cell
  //         case (false, false, false) => (cell.isLinked(West), cell.isLinked(South), cell.isLinked(East)) match {
  //           case (true, true, true) => upperRightCorner + " " * cellWidth + upperLeftCorner
  //           case (true, true, false) => upperRightCorner + " " * cellWidth + fourWayJuncture
  //           case (true, false, true) => horizontalLine + horizontalLine * cellWidth + horizontalLine
  //           case (false, true, true) => fourWayJuncture + horizontalLine * cellWidth + horizontalLine
  //           case (true, false, false) => horizontalLine + horizontalLine * cellWidth + fourWayJuncture
  //           case (false, true, false) => fourWayJuncture + " " * cellWidth + fourWayJuncture
  //           case (false, false, true) => fourWayJuncture + horizontalLine * cellWidth + horizontalLine
  //           case (false, false, false) => fourWayJuncture + horizontalLine * cellWidth + fourWayJuncture
  //         }
  //       }
  //       // always draw the right wall
  //       val cellRightWall: String = (isNorthWall, isNortheastCorner, isEastWall, isSoutheastCorner, isSouthWall, isSouthwestCorner, isWestWall, isNorthwestCorner) match {
  //         // north wall 
  //         case (true, _, _, _, _, _, _, _) => "o" //???
  //         // northeast corner 
  //         case (_, true, _, _, _, _, _, _) => "o" //???
  //         // east wall 
  //         case (_, _, true, _, _, _, _, _) => "o" //???
  //         // southeast corner 
  //         case (_, _, _, true, _, _, _, _) => "o" //???
  //         // south wall 
  //         case (_, _, _, _, true, _, _, _) => "o" //???
  //         // southwest corner
  //         case (_, _, _, _, _, true, _, _) => "o" //???
  //         // west wall 
  //         case (_, _, _, _, _, _, true, _) => "o" //???
  //         // northwest wall 
  //         case (_, _, _, _, _, _, _, true) => "o" //???
  //         // typical case: in middle somewhere
  //         case (false, false, false, false, false, false, false, false) => "o" //???
  //       }
  //       topAcc +=    cellTopWall
  //       middleAcc += cellLeftWall + cell + cellRightWall
  //       bottomAcc += cellBottomWall
  //     }
  //     acc += "TOP:    " + topAcc + "\n" + middleAcc + "\n" + bottomAcc + "\n"
  //     acc += "MIDDLE: " + middleAcc + "\n" + bottomAcc + "\n"
  //     acc += "BOTTOM: " + bottomAcc + "\n"
  //   }
  //   acc
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
  //   // var output: String = "+" + horizontalLineThick * 4 * columns + "\n"
  //   var output: String = "\u25C6" + horizontalLineThick * 4 * columns + "\n"
  //   for (row <- cells) {
  //     // "top" represents the vertical portions of the cell we're drawing
  //     var top: String = verticalLineThick 
  //     // "bottom" represents the horizontal portion of the cell we're drawing 
  //     var bottom: String = ""
  //     for (cell <- row) {
  //       if (bottom.length() == 0) {
  //         // first character of bottom; other characters will be appended 
  //         val cellLeftVerticalWall: String = (cell.neighbors.south, cell.isLinked(South)) match {
  //           case (None, false) => southWestCornerThick // bottom left corner of grid
  //           case (Some(neighbor), false) => threeWayJunctureEastwardThick // no vertical linkage, so 3-way juncture
  //           case (_, true) => verticalLineThick // vertical linkage, so no juncture here
  //         }
  //         bottom += cellLeftVerticalWall
  //       }
  //       val body = "   " // "body" represents the empty space inside the cell
  //       val cellRightVerticalWall: String = cell.isLinked(East) match {
  //         case true => " "
  //         case false => verticalLineThick
  //       }
  //       top += body + cellRightVerticalWall
  //       val cellBottomHorizontalWall: String = cell.isLinked(South) match {
  //         case true => " " * 3
  //         case false => horizontalLineThick * 3
  //       }
  //       // val cellBottomRightCorner: String = "\u25AA"
  //       val cellBottomRightCorner: String = "\u25C6"
  //       bottom += cellBottomHorizontalWall + cellBottomRightCorner
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
